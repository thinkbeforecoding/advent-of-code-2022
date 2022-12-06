.data

/* Data segment: define our message string and calculate its length. */
s_notfound:
    .ascii        "Not Found\n"
l_notfound = . - s_notfound

million =  1000000
mem:

.text

/* Our application's entry point. */
.globl _start
_start:
    mov     x11, #14        /* x11 is the size of the window, 14 for part 2 */
                            /* change it to 4 for part 1 */

/* Read STDIN and copy it at mem address */
    ldr     x4, =mem        /* x4 is an pointer to load stdin by chunks of 1024 bytes */ 
loop:
    add     x0,x4, #1024    /* reserve 1024 bytes */
    mov     w8, #214        /* brk: extende data memory to x0 */
    svc     #0

    mov    x0, #0           /* #0 is Stdint */
    mov    x1, x4           /* read and copy at x4 */
    mov    x2, #1024        /* size of the read */
    mov    w8, #63          /* read is syscall #63 */
    svc    #0

    add     x4, x4, x0      /* on return, x0 is actual read size */
    cmp     x0, #1024       /* if we read the full buffer, try again */
    b.eq   loop    



                            /* x4 is now end of string */
    ldr     x5, =mem        /* x5 is pointer to current start */
check_window:               /* check if the window contains only distinct chars */
    add     x6, x5, x11     /* x6 is end of span */
    cmp     x6, x4          /* check if the window pass the end of the string */
    b.ge    notfound        /* in this case, we did not find */

    mov     x7, x5          /* x7 is pointer in span */
loop_x:
    add     x8, x7, #1      /* x8 is other pointer in span */
    cmp     x8, x6          /* are we at the end of span ? */
    b.eq    found           /* yes: the span contains no duplicate char */
loop_y:
    ldrb    w9, [x7,0]      /* load char at x7 */
    ldrb    w10, [x8,0]     /* load char at x8 */
    cmp     w9, w10         /* are the equal ? */
    b.eq    move_window     /* yes: we can move the window */

    add     x8 , x8, #1     /* same x7, but next x8 */
    cmp     x8,x6           /* x8 reached end of span */
    b.ne    loop_y          /* no: just continue to check chars */
                            
    add     x7,x7,#1        /* yes: increment x7, x8 is reset after loop */
    bl      loop_x

move_window:
    add     x5,x5,#1        /* move the window by 1 char */
    bl      check_window

found:                      /* x5 point to the start of the window */
    add     x0 , x5 , x11   /* add window size to get end of window in x0 */       
    ldr     x6, =mem        /* x6 point to the start of the string */
    sub     x0, x0, x6      /* x0 = x0 - x6 is length we are looking for */

/*
    print the int result in x0 as a string  
*/
    mov     x2, #0          /* x7 is the length of the result in char */

    ldr     x8, =million    /* x8 is the current power of 10 */
    mov     x9, #10         /* x9 is just 10 for division */
findmult:                   /* divide x8 by ten until it's smaller than 
                              the value we want to print (x0) */
    udiv    x8, x8, x9      /* divide x8 by 10  */
    cmp     x8, x0          /* is it smaller than x0 ? */
    b.gt    findmult        /* no: divide again */
    mov     x1, x0          /* copy x0 in x1 to be compatible with print last */
    cmp     x0, #0          /* if x0 is 0, x8 is also 0, skip the division */
    b.eq    printLast
loopprint:    
    udiv    x1, x0, x8      /* x1 = x0 (remaining value) / x8 (power of 10)
                               so x1 is highest digit */    
printLast:
    add     x3, x1, #48     /* add 48 to conver to ascii char */
    strb    w3, [x6, x2]    /* store value in output string */
    add     x2,x2,#1        /* incr length */
    msub    x0, x1, x8, x0  /* subtract highest digit * (10 power of 10) */
    udiv    x8, x8, x9      /* next smaller power of 10 */
    cmp     x8, #0          /* are we finished ? */
    b.ne    loopprint
    strb    w9, [x6, x2]    /* '\n' is 10.. x9 is 10.. just use it for line return */ 
    add     x2 ,x2 ,#1      /* increase length by 1 to contain newline */

print:
    mov     x0, #1          /* fd := STDOUT_FILENO */
    ldr     x1, =mem        /* buf := msg */
    mov     w8, #64         /* write is syscall #64 */
    svc     #0              /* invoke syscall */


    /* syscall exit(int status) */
    mov     x0, #0
    mov     w8, #93         /* exit is syscall #93 */
    svc     #0              /* invoke syscall */

notfound:
    /* syscall write(int fd, const void *buf, size_t count) */
    mov     x0, #1          /* fd := STDOUT_FILENO */
    ldr     x1, =s_notfound /* buf := msg */
    ldr     x2, =l_notfound /* count := len */
    mov     w8, #64         /* write is syscall #64 */
    svc     #0              /* invoke syscall */

quit:
    /* syscall exit(int status) */
    mov     x0, #0          /* status := 0 */
    mov     w8, #93         /* exit is syscall #93 */
    svc     #0              /* invoke syscall */
