
bar = function(x)
    return "My test: " .. x
end

foo = function(x)
    return bar(x)
end

print(foo(10))