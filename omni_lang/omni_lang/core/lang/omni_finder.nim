# MIT License
# 
# Copyright (c) 2020 Francesco Cameli
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

import macros, strutils

macro findDatasAndStructs*(t : typed, is_ugen : typed = false) : untyped =
    result = nnkStmtList.newTree()

    let is_ugen_bool = is_ugen.boolVal()
    var t_type : NimNode
    if is_ugen_bool:
        t_type = nnkPtrTy.newTree(newIdentNode("UGen"))
    else:
        if t.kind != nnkIdent or t.kind != nnkSym:
            error("Not a valid object type!")
        t_type = newIdentNode(t.strVal())

    var 
        proc_def = nnkProcDef.newTree(
            newIdentNode("checkDatasAndStructsValidity"),
            newEmptyNode(),
            newEmptyNode(),
            nnkFormalParams.newTree(
                newIdentNode("bool"),
                nnkIdentDefs.newTree(
                    newIdentNode("obj"),
                    t_type,
                    newEmptyNode()
                )
            ),
            nnkPragma.newTree(
                newIdentNode("inline")
            ),
            newEmptyNode()
        )
        
        proc_body = nnkStmtList.newTree()
    
    var type_def : NimNode
    if is_ugen_bool:
        let type_impl = t.getImpl()
        if type_impl.len < 2:
            return
        type_def = type_impl[2]
    else:
        let type_impl = t.getType()[1][1]
        if type_impl.kind == nnkBracketExpr:
            type_def = (type_impl[0]).getTypeImpl()
        else:
            type_def = type_impl.getTypeImpl()
    
    var actual_type_def : NimNode

    #If it's a pointer, exctract
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
        error("Not a valid object type!")

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
        
        let var_name_kind = var_name.kind

        if var_name_kind != nnkIdent and var_name_kind != nnkSym:
            continue

        let type_to_inspect_string = type_to_inspect.strVal()

        let var_name_ident = newIdentNode(var_name.strVal())

        #Found a data
        if type_to_inspect_string == "Data" or type_to_inspect_string == "Data_obj":
            if var_type.kind != nnkBracketExpr:
                continue

            #Add the data itself first
            proc_body.add(
                nnkIfStmt.newTree(
                    nnkElifBranch.newTree(
                        nnkPrefix.newTree(
                            newIdentNode("not"),
                            nnkCall.newTree(
                                newIdentNode("checkDataValidity"),
                                nnkDotExpr.newTree(
                                    newIdentNode("obj"),
                                    var_name_ident
                                )
                            )
                        ),
                        nnkStmtList.newTree(
                            nnkReturnStmt.newTree(
                                newIdentNode("false")
                            )
                        )
                    )
                )
            )

            #Check if it's a Data[Data[Data[...]]]
            var interim_type = var_type

            var 
                final_stmt = nnkStmtList.newTree()
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
                    type_name : NimNode
                    is_data = false

                if data_content.kind == nnkBracketExpr:
                    type_name = data_content[0]
                    if type_name.strVal() == "Data" or type_name.strVal() == "Data_obj":
                        is_data = true
                        interim_type = data_content        
                else:
                    break

                let 
                    index_ident = newIdentNode("i" & $counter)
                    index_entry = newIdentNode("entry" & $counter)

                #If it hits a Data, add "checkDataValidity"
                if is_data:
                    if counter == 0:
                        let data_name = nnkDotExpr.newTree(
                            newIdentNode("obj"),
                            var_name
                        )

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
                            nnkIfStmt.newTree(
                                nnkElifBranch.newTree(
                                    nnkPrefix.newTree(
                                        newIdentNode("not"),
                                        nnkCall.newTree(
                                            newIdentNode("checkDataValidity"),
                                            index_entry
                                        )
                                    ),
                                    nnkStmtList.newTree(
                                        nnkReturnStmt.newTree(
                                            newIdentNode("false")
                                        )
                                    )
                                )
                            )
                        )

                        previous_loop_stmt = nnkForStmt.newTree(
                            index_ident,
                            nnkInfix.newTree(
                                newIdentNode(".."),
                                newLit(0),
                                nnkPar.newTree(
                                    nnkInfix.newTree(
                                        newIdentNode("-"),
                                        nnkCall.newTree(
                                            newIdentNode("size"),
                                            data_name
                                        ),
                                        newLit(1)
                                    )
                                )
                            ),
                            previous_body_stmt
                        )

                    else:
                        previous_body_stmt.add(
                            nnkForStmt.newTree(
                                index_ident,
                                nnkInfix.newTree(
                                    newIdentNode(".."),
                                    newLit(0),
                                    nnkPar.newTree(
                                        nnkInfix.newTree(
                                            newIdentNode("-"),
                                            nnkCall.newTree(
                                                newIdentNode("size"),
                                                prev_index_entry
                                            ),
                                            newLit(1)
                                        )
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
                                    nnkIfStmt.newTree(
                                        nnkElifBranch.newTree(
                                            nnkPrefix.newTree(
                                                newIdentNode("not"),
                                                nnkCall.newTree(
                                                    newIdentNode("checkDataValidity"),
                                                    index_entry
                                                )
                                            ),
                                            nnkStmtList.newTree(
                                                nnkReturnStmt.newTree(
                                                    newIdentNode("false")
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        
                        #Update
                        previous_body_stmt = previous_body_stmt[2][2]

                    prev_index_ident = index_ident
                    prev_index_entry = index_entry
                    
                #If it hits a struct add "checkDatasAndStructsValidity" and exit the loop
                else:
                    previous_body_stmt.add(
                        nnkForStmt.newTree(
                            index_ident,
                            nnkInfix.newTree(
                                newIdentNode(".."),
                                newLit(0),
                                nnkPar.newTree(
                                    nnkInfix.newTree(
                                        newIdentNode("-"),
                                        nnkCall.newTree(
                                            newIdentNode("size"),
                                            prev_index_entry
                                        ),
                                        newLit(1)
                                    )
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
                                nnkIfStmt.newTree(
                                    nnkElifBranch.newTree(
                                        nnkPrefix.newTree(
                                            newIdentNode("not"),
                                            nnkCall.newTree(
                                                newIdentNode("checkDatasAndStructsValidity"),
                                                index_entry
                                            )
                                        ),
                                        nnkStmtList.newTree(
                                            nnkReturnStmt.newTree(
                                                newIdentNode("false")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                    
                    #Exit loop!
                    break
                
                #Increat index counter
                counter += 1
                if counter >= max_count:
                    error("Infinite type inference loop")
            
            #Add the thingy to result
            if previous_loop_stmt != nil:
                proc_body.add(previous_loop_stmt)

        #Found a struct
        elif type_to_inspect_string.endsWith("_obj"):
            proc_body.add(
                nnkIfStmt.newTree(
                    nnkElifBranch.newTree(
                        nnkPrefix.newTree(
                            newIdentNode("not"),
                            nnkCall.newTree(
                                newIdentNode("checkDatasAndStructsValidity"),
                                nnkDotExpr.newTree(
                                    newIdentNode("obj"),
                                    var_name_ident
                                )
                            )
                        ),
                        nnkStmtList.newTree(
                            nnkReturnStmt.newTree(
                                newIdentNode("false")
                            )
                        )
                    )
                )
            )

    #Add all the stuff to the result
    proc_body.add(
        nnkReturnStmt.newTree(
            newIdentNode("true")
        )
    )
    proc_def.add(proc_body)
    result.add(proc_def)