import macros

macro def*(function_signature : untyped, code_block : untyped) : untyped =
    var 
        proc_and_template = nnkStmtList.newTree()

        proc_def = nnkProcDef.newTree()
        proc_return_type : NimNode
        proc_name : NimNode
        proc_generic_params = nnkGenericParams.newTree()
        proc_formal_params  = nnkFormalParams.newTree()

        template_def = nnkTemplateDef.newTree()
        template_name : NimNode
        template_body_call = nnkCall.newTree()

    #Pass the proc body to the parse_block_for_variables macro to avoid var/let declarations!!!
    var proc_body = nnkStmtList.newTree(
            nnkCall.newTree(
                newIdentNode("parse_block_for_variables"),
                code_block
            )
        )   
    
    let function_signature_kind = function_signature.kind

    if function_signature_kind == nnkCommand or function_signature_kind == nnkObjConstr or function_signature_kind == nnkCall or function_signature_kind == nnkInfix:
        
        var name_with_args : NimNode

        #Missing the return type entirely OR not providing any infos at all.
        #Defaults to returning auto. This also allows void for no return specified.
        if function_signature_kind == nnkObjConstr or function_signature_kind == nnkCall:
            name_with_args = function_signature
            proc_return_type = newIdentNode("auto")
        
        elif function_signature_kind == nnkCommand:
            name_with_args   = function_signature[0]
            proc_return_type = function_signature[1]
        
        elif function_signature_kind == nnkInfix:
            
            if function_signature[0].strVal() != "->":
                error "def: invalid return operator: \"" & $function_signature[0] & "\". Use \"->\"."
            
            name_with_args   = function_signature[1]
            proc_return_type = function_signature[2]

        let first_statement = name_with_args[0]
        
        #Generics
        if first_statement.kind == nnkBracketExpr:
            for index, entry in first_statement.pairs():
                #Name of function
                if index == 0:
                    proc_name = entry
                    continue

                if entry.kind == nnkExprColonExpr:
                    error "def: can't specify generics value \"" & $entry[0].strVal & " : " & $entry[1].strVal & "\" for \"def " & $proc_name.strVal & "\". It is defaulted to be \"SomeNumber\"."
                
                #Generics (for now) can only be SomeNumber
                proc_generic_params.add(
                    nnkIdentDefs.newTree(
                        entry,
                        newIdentNode("SomeNumber"),
                        newEmptyNode()
                    )
                )
        
        #No Generics
        elif first_statement.kind == nnkIdent:
            proc_name = first_statement

        #Formal params
        proc_formal_params.add(proc_return_type)    

        #Add template and proc names
        template_name = proc_name
        proc_name = newIdentNode(proc_name.strVal() & "_inner")
        
        #Add proc name to template call
        template_body_call.add(proc_name)
        
        let args_block = name_with_args[1..name_with_args.len-1]
    
        for index, statement in args_block.pairs():
            
            var 
                arg_name : NimNode
                new_arg : NimNode

            let statement_kind = statement.kind

            #a float = 0.5 -> a : float = 0.5 / a = 0.5 -> a : auto = 0.5
            if statement_kind == nnkExprEqExpr:                
                assert statement.len == 2

                var 
                    arg_type : NimNode
                    arg_value : NimNode

                #a float = 0.5
                if statement[0].kind == nnkCommand:
                    assert statement[0].len == 2
                    
                    arg_name = statement[0][0]
                    arg_type = statement[0][1]
                
                #a = 0.5
                else:
                    arg_name = statement[0]
                    arg_type = newIdentNode("auto")
                
                arg_value = statement[1]

                new_arg = nnkIdentDefs.newTree(
                    arg_name,
                    arg_type,
                    arg_value
                )
            
            #a float -> a : float
            elif statement_kind == nnkCommand:
                
                assert statement.len == 2

                arg_name = statement[0]

                new_arg = nnkIdentDefs.newTree(
                    arg_name,
                    statement[1],
                    newEmptyNode()
                )
            
            #a -> a : auto
            elif statement_kind == nnkIdent:
                arg_name = statement
                new_arg = nnkIdentDefs.newTree(
                    arg_name,
                    newIdentNode("auto"),
                    newEmptyNode()
                )

            else:
                error("\"def " & $proc_name.strVal() & "\": Invalid argument, \"" & $(repr statement) & "\"")

            proc_formal_params.add(new_arg)

            #Add arg name to template call
            template_body_call.add(arg_name)
        
        # ========== #
        # BUILD PROC #
        # ========== #

        #Add name of func (with _inner appended) with * for export
        proc_def.add(
            nnkPostfix.newTree(
                newIdentNode("*"),
                proc_name
            )
        )

        #Add generics
        if proc_generic_params.len > 0:
            proc_def.add(newEmptyNode())
            proc_def.add(proc_generic_params)
        else:
            proc_def.add(newEmptyNode())
            proc_def.add(newEmptyNode())

        #Add ugen_auto_mem : ptr OmniAutoMem
        proc_formal_params.add(nnkIdentDefs.newTree(
                newIdentNode("ugen_auto_mem"),
                nnkPtrTy.newTree(
                    newIdentNode("OmniAutoMem")
                ),
                newEmptyNode()
            )
        )

        #Add formal args
        proc_def.add(proc_formal_params)
        
        #Add inline pragma
        proc_def.add(nnkPragma.newTree(
                newIdentNode("inline")
            )
        )   

        proc_def.add(newEmptyNode())
        
        #Add function body (with checks for var/lets macro)
        proc_def.add(proc_body)

        # ============== #
        # BUILD TEMPLATE #
        # ============== #

        #Add name with * for export
        template_def.add(
            nnkPostfix.newTree(
                newIdentNode("*"),
                template_name
            )
        )

        #Add generics
        if proc_generic_params.len > 0:
            template_def.add(newEmptyNode())
            template_def.add(proc_generic_params)
        else:
            template_def.add(newEmptyNode())
            template_def.add(newEmptyNode())

        #Add formal args (Removing the last one, which is ugen_auto_mem : ptr OmniAutoMem, and substituting the first one (the return type) with "untyped")
        let template_formal_params = proc_formal_params.copy
        template_formal_params.del(template_formal_params.len - 1)
        template_formal_params[0] = newIdentNode("untyped")
        template_def.add(template_formal_params)
        template_def.add(newEmptyNode())
        template_def.add(newEmptyNode())

        #Add ugen_auto_mem to template call
        template_body_call.add(newIdentNode("ugen_auto_mem"))
        
        #Add body (just call _inner proc, adding "ugen_auto_mem" at the end)
        template_def.add(
            nnkStmtList.newTree(
                template_body_call
            )
        )

        #echo astGenRepr proc_def
        #echo repr proc_def 
        #echo repr template_def       
             
    else:
        error "Invalid syntax for def"

    proc_and_template.add(proc_def)
    proc_and_template.add(template_def)

    return proc_and_template