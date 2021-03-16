# MIT License
# 
# Copyright (c) 2020-2021 Francesco Cameli
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import macros, strutils, omni_type_checker

macro omni_generate_check_datas_validity*(t : typed, is_ugen : typed = false) : untyped =
    result = nnkStmtList.newTree()

    let is_ugen_bool = is_ugen.boolVal()
    var t_type : NimNode
    
    #Omni_UGen (top)
    if is_ugen_bool:
        t_type = newIdentNode("Omni_UGen")

    #struct     
    else:
        if t.kind != nnkIdent and t.kind != nnkSym:
            error("omni_generate_check_datas_validity: Not a valid object type!")
        
        t_type = newIdentNode(
            t.strVal()
        )

    var 
        proc_def = nnkProcDef.newTree(
            nnkPostfix.newTree(
                newIdentNode("*"),
                newIdentNode("omni_check_datas_validity")
            ),
            newEmptyNode(),
            newEmptyNode(),
            nnkFormalParams.newTree(
                newIdentNode("void"),
                nnkIdentDefs.newTree(
                    newIdentNode("omni_obj"),
                    t_type,
                    newEmptyNode()
                ),
                nnkIdentDefs.newTree(
                    newIdentNode("samplerate"),
                    newIdentNode("float"),
                    newEmptyNode()
                ),
                nnkIdentDefs.newTree(
                    newIdentNode("bufsize"),
                    newIdentNode("int"),
                    newEmptyNode()
                ),
                nnkIdentDefs.newTree(
                    newIdentNode("omni_auto_mem"),
                    newIdentNode("Omni_AutoMem"),
                    newEmptyNode()
                ),
                nnkIdentDefs.newTree(
                        newIdentNode("omni_call_type"),
                        nnkBracketExpr.newTree(
                            newIdentNode("typedesc"),
                            newIdentNode("Omni_CallType")
                        ),
                        newIdentNode("Omni_InitCall")
                    )
            ),
            nnkPragma.newTree(
                newIdentNode("inline")
            ),
            newEmptyNode()
        )
        
        proc_body = nnkStmtList.newTree()
    
    var type_def : NimNode
    #Omni_UGen
    if is_ugen_bool:
        let type_impl = t.getImpl()
        if type_impl.len < 2:
            return
        type_def = type_impl[2]

    #omni_struct
    else:
        let type_impl = t.getType()[1][1]
        if type_impl.kind == nnkBracketExpr:
            type_def = (type_impl[0]).getTypeImpl()
        else:
            type_def = type_impl.getTypeImpl()
    
    var actual_type_def : NimNode

    #If it's a pointer, extract it
    if type_def.kind == nnkPtrTy:   
        #if generic
        if type_def[0].kind == nnkBracketExpr:
            actual_type_def = getTypeImpl(type_def[0][0])
        else:
            actual_type_def = getTypeImpl(type_def[0])
    #Pass the definition through
    else:
        actual_type_def = type_def

    #If it's not an object type, abort the search.
    if actual_type_def.kind != nnkObjectTy:
        error("omni_generate_check_datas_validity: Not a valid object type!")

    let rec_list = actual_type_def[2]

    for ident_defs in rec_list:
        var
            var_name = ident_defs[0]
            var_type = ident_defs[1]
        
        var type_to_inspect : NimNode

        #if ptr
        if var_type.kind == nnkPtrTy:
            var_type = var_type[0]
        
        #if generic
        if var_type.kind == nnkBracketExpr:
            type_to_inspect = var_type[0]
        else:
            type_to_inspect = var_type

        #Tuples only support numbers for now, so no need to check for Datas
        #and Structs... Otherwise, they should be checked here!!!
        if type_to_inspect.kind == nnkTupleConstr:
            continue
        
        let var_name_kind = var_name.kind

        if var_name_kind != nnkIdent and var_name_kind != nnkSym:
            continue

        let type_to_inspect_string = type_to_inspect.strVal()

        let var_name_ident = newIdentNode(var_name.strVal())

        #Found a data
        if type_to_inspect_string == "Data" or type_to_inspect_string == "Data_omni_struct" or type_to_inspect_string == "Data_omni_struct_ptr":
            if var_type.kind != nnkBracketExpr:
                continue

            #Add the data itself first
            proc_body.add(
                nnkCall.newTree(
                    newIdentNode("omni_check_datas_validity"),
                    nnkDotExpr.newTree(
                        newIdentNode("omni_obj"),
                        var_name_ident,
                    ),
                    newIdentNode("samplerate"),
                    newIdentNode("bufsize"),
                    newIdentNode("omni_auto_mem"),
                    newIdentNode("omni_call_type")
                )
            )

            #Check if it's a Data[Data[Data[...]]]
            var interim_type = var_type

            var 
                previous_loop_stmt : NimNode
                previous_body_stmt : NimNode
                prev_index_ident : NimNode
                prev_index_entry : NimNode
            
            var 
                counter = 0
                max_count = 10000

            while(true):
                var 
                    data_content = interim_type[1]
                    data_content_kind = data_content.kind
                    type_name : NimNode
                    is_data = false

                if data_content_kind == nnkBracketExpr:
                    type_name = data_content[0]
                    let type_name_str = type_name.strVal()
                    if type_name_str == "Data" or type_name_str == "Data_omni_struct" or type_name_str == "Data_omni_struct_ptr":
                        is_data = true
                        interim_type = data_content   
                
                elif data_content_kind == nnkSym or data_content_kind == nnkIdent:
                    #Check for structs, otherwise, get out!
                    if not omni_is_struct(data_content):
                        break
                else:
                    break

                let data_name = nnkDotExpr.newTree(
                    newIdentNode("omni_obj"),
                    var_name
                )

                let 
                    index_ident = newIdentNode("i" & $counter)
                    index_entry = newIdentNode("entry" & $counter)

                #If it hits a Data, add "omni_check_datas_validity" for each entry!
                #for i in 0..<data.size: omni_check_datas_validity(data[i])
                if is_data:
                    if counter == 0:
                        previous_body_stmt = nnkStmtList.newTree(
                            nnkLetSection.newTree(
                                nnkIdentDefs.newTree(
                                    index_entry,
                                    newEmptyNode(),
                                    nnkBracketExpr.newTree(
                                        data_name,
                                        index_ident
                                    )
                                )
                            ),
                            nnkCall.newTree(
                                newIdentNode("omni_check_datas_validity"),
                                index_entry,
                                newIdentNode("samplerate"),
                                newIdentNode("bufsize"),
                                newIdentNode("omni_auto_mem"),
                                newIdentNode("omni_call_type")
                            )
                        )

                        previous_loop_stmt = nnkForStmt.newTree(
                            index_ident,
                            nnkInfix.newTree(
                                newIdentNode("..<"),
                                newLit(0),
                                nnkCall.newTree(
                                    newIdentNode("size"),
                                    data_name
                                )
                            ),
                            previous_body_stmt
                        )

                    else:
                        previous_body_stmt.add(
                            nnkForStmt.newTree(
                                index_ident,
                                nnkInfix.newTree(
                                    newIdentNode("..<"),
                                    newLit(0),
                                    nnkCall.newTree(
                                        newIdentNode("size"),
                                        prev_index_entry
                                    )
                                ),
                                nnkStmtList.newTree(
                                    nnkLetSection.newTree(
                                        nnkIdentDefs.newTree(
                                            index_entry,
                                            newEmptyNode(),
                                            nnkBracketExpr.newTree(
                                                prev_index_entry,
                                                index_ident
                                            )
                                        )
                                    ),
                                    nnkCall.newTree(
                                        newIdentNode("omni_check_datas_validity"),
                                        index_entry,
                                        newIdentNode("samplerate"),
                                        newIdentNode("bufsize"),
                                        newIdentNode("omni_auto_mem"),
                                        newIdentNode("omni_call_type")
                                    )
                                )
                            )
                        )
                        
                        #Update
                        previous_body_stmt = previous_body_stmt[2][2]

                    prev_index_ident = index_ident
                    prev_index_entry = index_entry

                #If it hits a struct add "omni_check_datas_validity" and exit the loop
                else:
                    if previous_body_stmt == nil:
                        prev_index_entry = data_name
                        prev_index_ident = index_ident
                        previous_body_stmt = nnkStmtList.newTree()
                        previous_loop_stmt = nnkStmtList.newTree(previous_body_stmt)

                    previous_body_stmt.add(
                        nnkForStmt.newTree(
                            index_ident,
                            nnkInfix.newTree(
                                newIdentNode("..<"),
                                newLit(0),
                                nnkCall.newTree(
                                    newIdentNode("size"),
                                    prev_index_entry
                                )
                            ),
                            nnkStmtList.newTree(
                                nnkLetSection.newTree(
                                    nnkIdentDefs.newTree(
                                        index_entry,
                                        newEmptyNode(),
                                        nnkBracketExpr.newTree(
                                            prev_index_entry,
                                            index_ident
                                        )
                                    )
                                ),
                                nnkCall.newTree(
                                    newIdentNode("omni_check_datas_validity"),
                                    index_entry,
                                    newIdentNode("samplerate"),
                                    newIdentNode("bufsize"),
                                    newIdentNode("omni_auto_mem"),
                                    newIdentNode("omni_call_type")
                                )
                            )
                        )
                    )
                    
                    #Exit loop!
                    break
                
                #Increase index counter
                counter += 1
                if counter >= max_count:
                    error("omni_check_datas_validity: Infinite type inference loop.")
            
            #Add to result
            if previous_loop_stmt != nil:
                proc_body.add(previous_loop_stmt)

        #Found a struct
        elif type_to_inspect_string.endsWith("_omni_struct") or type_to_inspect.omni_is_struct():
            proc_body.add(
                nnkCall.newTree(
                    newIdentNode("omni_check_datas_validity"),
                    nnkDotExpr.newTree(
                        newIdentNode("omni_obj"),
                        var_name_ident,
                    ),
                    newIdentNode("samplerate"),
                    newIdentNode("bufsize"),
                    newIdentNode("omni_auto_mem"),
                    newIdentNode("omni_call_type")
                )
            )

    #Add all the stuff to the result
    proc_body.add(
        nnkDiscardStmt.newTree(
            newEmptyNode()
        )
    )

    proc_def.add(proc_body)
    
    result.add(proc_def)
