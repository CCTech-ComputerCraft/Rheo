local lpeg = require("lulpeg")

local tArgs = {...}
fl = shell.resolve(tArgs[1])

local Scopes = { {} }
local Classes = { }
local privVars = { }
local pubVars = { }
local pubScope
local privScope
local assign = pub_assign
local inClass = false
local currentReturn = nil
local inMethod = false
local currentClass = nil
local nFl = {}

for i in string.gmatch(fs.getName(fl), "[^%.]+") do
    table.insert(nFl, i)
end

local fWe = nFl[1]

function eval_bool(expr)
    local expr1 = eval(expr[2])
    local operator = expr[3]
    local expr2 = eval(expr[4])

    if operator == "==" then
        return expr1 == expr2
    elseif operator == "!=" then
        return expr1 ~= expr2
    elseif operator == ">=" then
        return expr1 >= expr2
    elseif operator == "<=" then
        return expr1 <= expr2
    elseif operator == "<" then
        return expr1 < expr2
    elseif operator == ">" then
        return expr1 > expr2
    end
end

function eval_if(if_ast)
    local last_bool
    local isElse = false

    for i=1, #if_ast do
        if isElse == true then
            break
        end
        if if_ast[i] == "if" then
            if eval_bool(if_ast[i+1]) then
                last_bool = eval_bool(if_ast[i+1])
                return eval(if_ast[i+2])
            end
        elseif if_ast[i][1] == "elseif" then
            if last_bool then

            elseif eval_bool(if_ast[i][2]) then
                last_bool = eval_bool(if_ast[i][2])
                return eval(if_ast[i][3])
            end
        elseif if_ast[i][1] == "else" then
            if last_bool then
            else
                isElse = true
                return eval(if_ast[i][2])
            end
        end
    end
end

function eval_expr(expr)
    local accum = eval(expr[2])

    if type(accum) == "number" then

        for i=3, #expr, 2 do
            local operaotr = eval(expr[i])
            local expr2 = eval(expr[i+1])

            if type(expr2) == "number" then
                if operator == "+" then
                    accum = expr1 + expr2
                elseif operator == "-" then
                    accum = expr1 + expr2
                elseif operator == "*" then
                    accum = expr1 * epxr2
                elseif operator == "/" then
                    accum = expr1 / expr2
                end
            end
        end
        return accum
    end
end

function eval(ast)
    if inClass == false then
        if ast[1] == "list" then
        elseif ast[1] == "class" then
        elseif ast[1] == "import" then
        elseif ast[1] == "eol" then
        else
            throw("Java: Expected "..ast[1].." to be in class!")
        end
    elseif inMethod == false then
        if ast[1] == "public" then
        elseif ast[1] == "private" then
        elseif ast[1] == "void" then
        elseif ast[1] == "class" then
        elseif ast[1] == "boolean" then
        elseif ast[1] == "String" then
        elseif ast[1] == "int" then
        elseif ast[1] == "string_arr" then
        elseif ast[1] == "string_ref" then
        elseif ast[1] == "float" then
        elseif ast[1] == "list" then
        elseif ast[1] == "true" then
        elseif ast[1] == "false" then
        elseif ast[1] == "null" then
        elseif ast[1] == "expr" then
        elseif ast[1] == "term" then
        elseif ast[1] == "string" then
        elseif ast[1] == "eol" then
        elseif ast[1] == "classCall" then
        else
            throw("Java: expected "..ast[1].." to be in Method!")
        end
    end

    if type(ast) == "number" then
        return ast
    elseif type(ast) == "String" then
        return ast
    elseif ast[1] == "string_def" then
        return eval(ast[2])
    elseif ast[1] == "expr" or ast[1] == "term" then
        return eval_expr(ast)
    elseif ast[1] == "true" then
        return true
    elseif ast[1] == "false" then
        return false
    elseif ast[1] == "null" then
        return nil
    elseif ast[1] == "list" then
        local last = nil
        for i=2, #ast do
            last = eval(ast[i])
        end

        return last
    elseif ast[1] == "class" then
        PrivScope = { { } }
        PubScope = { { } }
        Classes[ast[2]] = { code = ast[3], PrivScope = PrivScope, PubScope = PubScope }

        if ast[2] == fWe then
            local oIc = inClass
            local oCC = currentClass
            inClass = true
            currentClass = ast[2]
            local code = eval(ast[3])
            Classes[ast[2]].PrivScope = PrivScope
            Classes[ast[2]].PubScope = PubScope
            inClass = oIc
            currentClass = oCC
            return code
        end
    elseif ast[1] == "classCall" then
        if #ast == 2 then
            if Classes[ast[2]] then
                local oC = currentClass
                currentClass = ast[2]
                local oPuS = pubScope
                local oPrS = privScope
                pubScope = Classes[currentClass].pubScope
                privScope = Classes[currentClass].privScope
                local code = eval(Classes[currentClass].code)
                currentClass = oC
                pubScope = oPus
                privScope = oPrS
            end
        elseif #ast == 3 then
            if Classes[ast[2]] then
                local oClass = currentClass
                local oPuS = PubScope
                local oPrS = PrivScope
                currentClass = ast[2]
                PubScope = Classes[currentClass].PubScope
                PrivScope = Classes[currentClass].PrivScope

                eval(Classes[currentClass].code)

                if ast[3][1] == "ref" then
                    local val = pub_lookup(ast[3])

                    if not val then
                        throw(ast[3][2].." in class "..currentClass.." does not exists or is privat! :"..oClass..":")
                    end
                    currentClass = oClass
                    PubScope = oPuS
                    PrivScope = oPrS
                    return val
                elseif ast[3][1] == "call" then
                    local call = eval(ast[3])
                    currentClass = oClass
                    PubScope = oPuS
                    PrivScope = oPrS
                    return call
                end
            end
        end
    elseif ast[1] == "ref" then
        local try = priv_lookup(ast)

        if not try then
            try = pub_lookup(ast)
        end

        return try
    elseif ast[1] == "void" then
        if ast[2][2] == "main" then
            local oMeth = inMethod
            inMethod = true
            local code = eval(ast[4])

            inMethod = oMeth
        else
            return assign(ast[2],
                    { args = ast[3],
                    type = "void",
                    code = ast[4],
                    scope = Scopes[#Scopes] })
        end
    elseif ast[1] == "boolean" then
        if #ast == 3 then
            local val = eval(ast[3])

            if type(val) == "boolean" then
                return assign(ast[2], val)
            else
                throw("Attempt to assign "..type(val).." as boolean", 0)
            end
        elseif #ast == 4 then
            return assign(ast[2],
                    {
                        type = "boolean",
                        args = ast[3],
                        code = ast[4],
                        scope = PubScope[#PubScope] })
        end
    elseif ast[1] == "String" then
        if #ast == 3 then
            local val = eval(ast[3])

            if type(val) == "String" then
                return assign(ast[2], val)
            elseif type(val) == "table" and val[1] == "String" then
                return assign(ast[2], val)
            else
                throw("Attempt to assign "..type(val).." as String")
            end
        elseif #ast == 4 then
            return assign( ast[2]
                    {
                        type = "String",
                        args = ast[3],
                        code = ast[4],
                        scope = PubScope[#pubScope] })
        end
    elseif ast[1] == "call" then
        local oldVal = currentReturn
        local method = eval(ast[2])

        if not method then
            throw("Java: unexpected method: "..ast[2][2])
        end

        if type(method) ~= "method" then
            throw("Java: unexpected method: "..ast[2][2])
        end

        scope = setmetatable({}, {__index=method.scope})

        for i, name in ipairs(method.args) do
            local val = eval(ast[3][i])

            if type(val) == name[1] then
                scope[name[2]] = val
            else
                throw("Java: "..name[1].." expected, got "..type(val))
            end
        end

        local nMeth = inMethod
        inMethod = true
        table.insert(PubScope, scope)
        local code = eval(method.code)
        local reVal = currentReturn
        currentReturn = oldVal
        table.remove(PubScope)

        if method.type == "void" then
            inMethod = nMeth
            return
        else
            if type(reVal) == method.type then
                inMethod = nMeth
                return reVal
            else
                throw("Java: expected "..method.type.." to return, got "..type(reVal))
            end
        end
    elseif ast[1] == "public" then
        local oldAs = assign
        assign = pub_assign
        local code = eval(ast[2])
        assign = oldAs
        return code
    elseif ast[1] == "private" then
        local oldAs = assign
        assign = priv_assign
        local code = eval(ast[2])
        assign = oldAs


        return code
    elseif ast[1] == "return" then
        currentReturn = eval(ast[2])
        return currentReturn
    elseif ast[1] == "if" then
        return eval_if(ast)
    elseif ast[1] == "arg" then

    elseif ast[1] == "doLua" then
        local lCode = eval(ast[2])

        if type(lCode) == "String" then
            local func, err = loadstring(lCode)

            if not err then
                func()
            else
                throw(err)
            end
        else
            throw("String expected for doLua, got"..type(code))
        end
    elseif ast[1] == "import" then
        local package = {}

        for i in string.gmatch(ast[2], "[^%.]+") do
            table.insert(package, i)
        end

        if package[1] == "java" then
            local pFile = package[#package]
            table.remove(package, 1)
            local package = table.concat(package, "/")

            local file = fs.open(settings.get("rheo.home").."/javaBin/lib/"..package..".java", "r")

            if file then
                local handle = file.readAll()
                file.close()

                local oFl = fl
                fl = fs.getDir(fl).."/"..pFile..".java"

                parser:match(handle)
                fl = oFl
            else
                throw("Could not import package: "..ast[2])
            end
        else
            local pFile = package[#package]
            local package = table.concat(package, "/")

            local file = fs.open(fs.getDir(fl).."/"..package..".java", "r")

            if file then
                local handle = file.readAll()
                file.close()

                local oFl = fl
                fl = fs.getDir(fl).."/"..pFile..".java"
                parser:match(handle)
                fl = oFl
            else
                throw("Could not import package: "..ast[2])
            end
        end
    elseif ast[1] == "this" then
        local priv = PrivScope[#PrivScope]
        local pub = PubScope[#PubScope]
        if #ast == 1 then
            return {"classCall" , ast[2]}
        else
            if ast[2][1] == "ref" then
                return eval(ast[2])
            elseif ast[2][1] == "call" then
                return eval(ast[2])
            end
        end
    elseif ast[1] == "join" then
        local str = {}

        for i=1, #ast[2] do
            local nStr = eval(ast[2][i][2])
            if type(nStr) ~= "String" then
                throw("String expected, got "..type(nStr))
            end
            table.insert(str, nStr)
        end

        return table.concat(str, jS)
    elseif ast[1] == "string_arr" then
        local len = tonumber(ast[2][2][2])

        if type(len) ~= "number" then
            throw("Number expected for length, got "..type(len))
        end

        local tab = {"String"}
        for i=1,len do
            table.insert(tab, i)
        end

        return tab
    elseif ast[1] == "eol" then
        curLine = curLine+1
    else
        error("unexpected, "..ast[1][1])
    end
end

function pub_assign(ref, value)
    local current = PubScope[#PubScope]
    for i = 2, #ref do
        local next_index = ref[i]
        if type(next_index) == "table" then
            next_index = eval(next_index)
        end

        if i == #ref then
            while current[next_index] and not rawget(current, next_index) do
                current = getmetatable(current).__index
            end

            current[next_index] = value
            return value
        else
            current = current[next_index]
        end
    end
end

function priv_assign(ref, value)
    local current = PrivScope[#PrivScope]
    for i = 2, #ref do
        local next_index = ref[i]
        if type(next_index) == "table" then
            next_index = eval(next_index)
        end

        if i == #ref then
            while current[next_index] and not rawget(current, next_index) do
                current = getmetatable(current).__index
            end

            current[next_index] = value
            return value
        else
            current = current[next_index]
        end
    end
end

function pub_lookup(ref)
    local current = PubScope[#PubScope]
    for i = 2, #ref do
        local next_index = ref[i]

        if type(next_index) == "table" then
            next_index = eval(next_index)
        end

        if current then
            current = current[next_index]
        else
            current = nil
        end
    end
    return current
end

function priv_lookup(ref)
    local current = PrivScope[#PrivScope]
    for i = 2, #ref do
        local next_index = ref[i]

        if type(next_index) == "table" then
            next_index = eval(next_index)
        end

        if current then
            current = current[next_index]
        else
            current = nil
        end
    end
    return current
end

local spc = lpeg.S("\t\n ")^0

local digit = lpeg.R("09")
local number = lpeg.C( (lpeg.P("-") + digit) *
                digit^0 *
                ( lpeg.P(".") * digit^0 )^-1 ) / tonumber * spc
local letter = lpeg.R("AZ","az")
local name = lpeg.C( letter*( digit+letter+"_")^0 ) * spc
local packname = lpeg.C( letter*( digit+letter+"."+"_")^0 ) * spc

local lparen = "(" * spc
local rparen = ")" * spc
local lcurly = "{" * spc
local rcurly = "}" * spc
local lbrack = "[" * spc
local rbrack = "]" * spc
local semicolon = ";" * spc

local expr_op = lpeg.C( lpeg.S("+-") ) * spc
local term_op = lpeg.C( lpeg.S("*/") ) * spc
local boolean = lpeg.C( lpeg.S("<>") + "<=" + ">=" + "==" + "!=" ) * spc
local doublequoted_string = lpeg.P('"') * lpeg.C( ( name+"."+" "+"+"+"/"+"!"+"§"+"$"+"%"+"&"+"("+")"+"="+"?"+"`"+"°"+"^"+"*"+"#"+"~"+"´"+"ä"+"ö"+"ü"+"²"+"³"+"{"+"}"+"["+"]"+"'"+":"+";"+","+"-"+"<"+">"+"|"+"µ"+"?"+"@")^0 ) * lpeg.P('"') * spc
local singlequoted_string = lpeg.P("'") * lpeg.C( ( name+"."+" "+"+"+"/"+"!"+"§"+"$"+"%"+"&"+"("+")"+"="+"?"+"`"+"°"+"^"+"*"+"#"+"~"+"´"+"ä"+"ö"+"ü"+"²"+"³"+"{"+"}"+"["+"]"+":"+";"+","+"-"+"\""+"<"+">"+"|"+"µ"+"?"+"@")^0 ) * lpeg.P("'") * spc
local string = singlequoted_string + doublequoted_string

local arg = lpeg.C( lpeg.P("String") + lpeg.P("int") + lpeg.P("boolean") + lpeg.P("float") ) * spc
local keywords = ( lpeg.P("import") + lpeg.P("if") + lpeg.P("else") + lpeg.P("class") + lpeg.P("public") + lpeg.P("String") + lpeg.P("int") + lpeg.P("float") + lpeg.P("boolean") + lpeg.P("private") + lpeg.P("void") + lpeg.P("print") + lpeg.P("true") + lpeg.P("false") + lpeg.P("null") + lpeg.P("doLua") + lpeg.P("this") + lpeg.P("return") + lpeg.P("join") ) * spc
local name = name - keywords

stmt = spc * lpeg.P{
    "LIST";
    LIST = lpeg.Ct( lpeg.Cc("list") *
                ( lpeg.V("STMTS") * spc )^0 ),
    STMT =
        ( lpeg.V("CALL") +
          lpeg.V("CLASSCALL") +
          lpeg.V("IMPORT") +
          lpeg.V("THIS") +
          lpeg.V("DOLUA") +
          lpeg.V("RETURN") ) * semicolon * spc,
    STMTS =
        lpeg.V("CLASS") +
        lpeg.V("VOID") +
        lpeg.V("PUBLIC") +
        lpeg.V("PRIVATE") +
        lpeg.V("BOOLEAN") +
        lpeg.V("STRING") +
        lpeg.V("IF") +
        lpeg.V("STMT"),
    REF = lpeg.Ct( lpeg.Cc("ref") * name * ( lbrack * lpeg.V("VAL") * rbrack )^0 ),
    EXPR = lpeg.Ct( lpeg.Cc("expr") * lpeg.V("TERM") * ( expr_op * lpeg.V("TERM") )^0 ),
    TERM = lpeg.Ct( lpeg.Cc("term") * lpeg.V("FACT") * ( term_op * lpeg.V("FACT") )^0 ),
    FACT =
            number +
            lparen * lpeg.V("EXPR") * rparen +
            lpeg.V("REF"),
    STRING_DEF = lpeg.Ct( lpeg.Cc("string_def") * string ),
    VAL = lpeg.V("CALL") + lpeg.V("CLASSCALL") + lpeg.V("REF") + lpeg.V("EXPR") + lpeg.V("STRING_DEF") + lpeg.V("BOOLS") + lpeg.V("THIS") + lpeg.V("JOIN") + lpeg.V("STRING_ARR"),
    IMPORT = lpeg.Ct( lpeg.C("import") * spc * packname ),
    PUBLIC = lpeg.Ct( lpeg.C("public") * spc * ( lpeg.V("VOID") + lpeg.V("CONSTRUCTOR") + lpeg.V("BOOLEAN") + lpeg.V("INT") + lpeg.V("STRING") + lpeg.V("FLOAT") ) ),
    PRIVATE = lpeg.Ct( lpeg.C("private") * spc * ( lpeg.V("VOID") + lpeg.V("CONSTRUCTOR") + lpeg.V("BOOLEAN") + lpeg.V("INT") + lpeg.V("STRING") + lpeg.V("FLOAT") ) ),
    CLASS = lpeg.Ct( lpeg.C("class") * spc * name * spc * lcurly * lpeg.V("LIST") * spc * rcurly ),
    VOID = lpeg.Ct( lpeg.C("void") * spc * lpeg.V("REF") * lparen * spc * lpeg.V("NAME_LIST") * spc * rparen * lcurly * lpeg.V("LIST") * spc * rcurly ),
    CONSTRUCTOR = lpeg.Ct( lpeg.Cc("constructor") * lpeg.V("REF") * lparen * lpeg.V("NAME_LIST") * spc * rparen * lcurly * lpeg.V("LIST") * spc * rcurly ),
    BOOLEAN = lpeg.V("BOOLEAN_ASSIGN") + lpeg.Ct( lpeg.C("boolean") * spc * lpeg.V("REF") * lparen * lpeg.V("NAME_LIST") * spc * rparen * spc * lcurly * spc * lpeg.V("LIST") * spc * rcurly ),
    BOOLEAN_ASSIGN = lpeg.Ct( lpeg.C("boolean") * spc * lpeg.V("REF") * spc * "=" * spc * lpeg.V("VAL") * semicolon ),
    STRING = lpeg.V("STRING_ASSIGN") + lpeg.Ct( lpeg.C("String") * spc * lpeg.V("REF") * lparen * lpeg.V("NAME_LIST") * spc * rparen * spc * lcurly * lpeg.V("LIST") * spc * rcurly ),
    STRING_ASSIGN = lpeg.Ct( lpeg.C("String") * spc * lpeg.V("REF") * spc * "=" * spc * lpeg.V("VAL") * semicolon ),
    STRING_ARR = lpeg.Ct( lpeg.Cc("string_arr") * "String" * lbrack * spc * lpeg.V("VAL") * spc * rbrack ),
    INT = lpeg.Ct( lpeg.C("int") * spc * lpeg.V("REF") * lparen * lpeg.V("NAME_LIST") * spc * rparen * lcurly * lpeg.V("LIST") * spc * rcurly ),
    FLOAT = lpeg.Ct( lpeg.C("float") * spc * lpeg.V("REF") * lparen * lpeg.V("NAME_LIST") * spc * rparen * lcurly * lpeg.V("LIST") * spc * rcurly ),
    CALL = lpeg.Ct( lpeg.Cc("call") * lpeg.V("REF") * lparen * lpeg.V("VAL_LIST") * spc * rparen ),
    IF = lpeg.Ct( lpeg.C("if") * spc * lparen * spc * lpeg.V("BOOL") * spc * rparen * spc * lcurly * spc * lpeg.V("LIST") * spc * rcurly * spc * ( lpeg.V("ELSE_IF") + lpeg.V("ELSE") )^0 ),
    ELSE_IF = lpeg.Ct( lpeg.Cc("elseif") * "else if" * spc * lparen * spc * lpeg.V("BOOL") * spc * rparen * spc * lcurly * spc * lpeg.V("LIST") * spc * rcurly ),
    ELSE = lpeg.Ct( lpeg.C("else") * spc * lcurly * spc * lpeg.V("LIST") * spc * rcurly ),
    BOOL = lpeg.Ct( lpeg.Cc("bool") * lpeg.V("VAL") * spc * boolean * lpeg.V("VAL") ),
    BOOLS = lpeg.V("TRUE") + lpeg.V("FALSE") + lpeg.V("NULL"),
    TRUE = lpeg.Ct( lpeg.C("true") ),
    FALSE = lpeg.Ct( lpeg.C("false") ),
    NULL = lpeg.Ct( lpeg.C("null") ),
    RETURN = lpeg.Ct( lpeg.C("return") * spc * lpeg.V("VAL") ),
    ARGS = lpeg.Ct( arg * name ),
    DOLUA = lpeg.Ct( lpeg.C("doLua") * lparen * spc * lpeg.V("VAL") * spc * rparen ),
    THIS = lpeg.Ct( lpeg.C("this") * ( ( "." * ( lpeg.V("CALL") + lpeg.V("REF") ) ) + spc ) ),
    CLASSCALL = lpeg.Ct( lpeg.Cc("classCall") * name * ( ( "." * ( lpeg.V("CALL") + lpeg.V("REF") ) ) + spc ) ),
    JOIN = lpeg.Ct( lpeg.C("join") * lparen * spc * lpeg.V("VAL_LIST") * spc * rparen ),
    VAL_LIST = lpeg.Ct( ( lpeg.V("VAL") * ( "," * spc)^-1 * spc )^0 ),
    NAME_LIST = lpeg.Ct( ( lpeg.V("ARGS") * ( "," * spc)^0 )^0 ),
    EOL = lpeg.Ct( lpeg.Cc("eol") * "%%#n" )
}

oldType = type

function type(val)
    if oldType(val) == "table" then
        if val.type and val.args and val.scope and val.code then
            return "method"
        else
            return "table"
        end
    elseif Classes[val] then
        return "class"
    elseif oldType(val) == "nil" then
        return "null"
    elseif oldType(val) == "string" then
        return "String"
    else
        return oldType(val)
    end
end

function throw(msg)
    error(fl..": "..curLine..": "..msg, 0)
end

parser = stmt / eval
file = io.open(fl, "r")

if file then
    curLine = 1
    lines = {}

    for line in file:lines() do
        table.insert(lines, line)
    end

    file:close()

    code = table.concat(lines, " ")
    parser:match(code)
else
    error(fl.." does not exists!",0)
end
