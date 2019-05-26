local dict = {}

dict.parse = function(dc)
    if type(dc) == "table" then
        local result = {}
        for k, v in pairs(dc) do
            table.insert(result, k.." => "..v)
        end
        return table.concat(result, "\n")
    elseif type(dc) == "string" then
        local result = {}
        local resultV = {}
        local resultK = {}
        for i in string.gmatch(dc, "%S+") do
            table.insert(result, i)
        end
        
        for k, v in pairs(result) do
            if v == "=>" then
                table.remove(result, k)
            else
                table.insert(resultK, v)
                table.insert(resultV, v)
            end
        end
        
        for i=0, #resultV do
            i = i+1
            table.remove(resultV, i)
        end
        
        for i=1, #resultK do
            i = i+1
            table.remove(resultK, i)
        end
        
        local result = {}
        for i=1, #resultK do
            result[resultK[i]] = resultV[i]
        end
    else
        error("table or string to parse expected, got"..type(dc))
    end
end

return dict