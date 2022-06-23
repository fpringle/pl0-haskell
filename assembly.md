Instruction Set
===============

- MOV a a/v
    - move an address or value to an address
    - e.g.
        - MOV 0x1 0x0       -- move the value at address 0 to address 1
        - MOV 0x2 20        -- set address 2 to 20

- JNZ a/v l
    - jump to label l if address a or value v is not equal to 0
    - e.g
        - JNZ 0x1 LOOP      -- if address 1 is not zero, jump to label "LOOP"
        - JNZ 1 MAIN        -- jump to label "MAIN" unconditionally

- ADD a a/v a/v
    - add 2 values and store the result at an address
    - e.g.
        - ADD 0x2 0x0 0x1   -- add the value at 0 to the value at 1, store at 2
        - ADD 0x5 0x0 1     -- add 1 to the value at 0 and store at 5

- SUB a a/v a/v
    - subtract 2 values and store the result at an address
    - e.g.
        - SUB 0x2 0x0 0x1   -- subtract the value at 1 from the value at 0, store at 2
        - SUB 0x5 0x0 1     -- subtract 1 from the value at 0 and store at 5

- MUL a a/v a/v
    - multiply 2 values and store the result at an address
    - e.g.
        - MUL 0x2 0x0 0x1   -- multiply the value at 0 by the value at 1, store at 2
        - MUL 0x5 0x0 2     -- double the value at 0 and store at 5

- DIV a a/v a/v
    - divide 2 values and store the result at an address
    - e.g.
        - DIV 0x2 0x0 0x1   -- divide the value at 0 by the value at 1, store at 2
        - DIV 0x5 0x0 2     -- halve the value at 0 and store at 5

- NOT a a/v
    - take the logical inverse (0->1, x->0) of a value and store the result at an address
    - e.g.
        - NOT 0x0 0x1       -- if value at 0x1 is zero then store 0 at addr0, else 1
        - NOT 0x0 50        -- set addr0 to 0

- X a a/v a/v where X is one of LT, LTE, EQ, NEQ
    - compare 2 values and store the result at an address (1 = true, 0 = false)
    - e.g.
        - LT 0x2 0x0 0x1    -- if the value at 0 is less than the value at 1, set addr 2 to 1, else 0
        - EQ 0x5 0x0 2      -- if the value at 0 is equal 2 to then set addr 5 to 1, else 0

- PRINT a/v
    - print a value or address

- INPUT a
    - read a value from stdin and store at address
