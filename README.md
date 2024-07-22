# dont use this
## code has lots of mem leaks i think and uhm is not ready
## lots of ast stuff is still not compilable and yeah uhm 

# follows lua 5.1 spec
## eh--- almost

Differences with Lua 5.1:
- You can return only one value
- Functions inside functions are not allowed
- When there is no return, it returns nil
- Table are tables {}, Arrays are Array (index start at 0 btw :3)
- You can do stuff after return
- No ElseIf
- No Methods for function calls
- No meta methods
- Bit operations (like in lua 5.4)
- Only actual args x(a), x"yo" or x{} are not allowed
- No variadic args
- No local functions
- more comming i dont know