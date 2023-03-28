\ Data and Return stack allocation, four pairs of data and return stacks
\ Note : the return stack must be in the lowest 64K byte segment, for the BIOS calls to work.
$0400 constant DATA_STACK_SIZE_0    \ 
$0500 constant DATA_STACK_SIZE_1    \ must be > $400 for colorForth "Life" program to work
$0100 constant DATA_STACK_SIZE_2    \ 
$0100 constant DATA_STACK_SIZE_3    \ 
$0100 constant DATA_STACK_SIZE_GAP  \ leave space under the last data stack to check for underflow 

$0100 constant RETURN_STACK_SIZE

\ return stacks
$7800 constant RETURN_STACK_0      \ top of stack memory area 
RETURN_STACK_0 RETURN_STACK_SIZE - constant RETURN_STACK_1
RETURN_STACK_1 RETURN_STACK_SIZE - constant RETURN_STACK_2
RETURN_STACK_2 RETURN_STACK_SIZE - constant RETURN_STACK_3

\ data stacks
RETURN_STACK_3 RETURN_STACK_SIZE - constant DATA_STACK_0      
DATA_STACK_0   DATA_STACK_SIZE_0 - constant DATA_STACK_1    \ BIG data stack for the show task
DATA_STACK_1   DATA_STACK_SIZE_1 - constant DATA_STACK_2
DATA_STACK_2   DATA_STACK_SIZE_2 - constant DATA_STACK_3
DATA_STACK_3   DATA_STACK_SIZE_3 - DATA_STACK_SIZE_GAP - constant STACK_MEMORY_START

\ four pairs of stacks, one for each task   
RETURN_STACK_0 STACK_MEMORY_START - constant TOTAL_STACK_SIZE                        

: tt_showStacks ( -- )
   cr." return stacks : "                       
   cr."    RETURN_STACK_0  = "  RETURN_STACK_0  4.hex
   cr."    RETURN_STACK_1  = "  RETURN_STACK_1  4.hex
   cr."    RETURN_STACK_2  = "  RETURN_STACK_2  4.hex
   cr."    RETURN_STACK_3  = "  RETURN_STACK_3  4.hex 
   cr
   cr." data stacks : " 
   cr."    DATA_STACK_0    = "  DATA_STACK_0    4.hex
   cr."    DATA_STACK_1    = "  DATA_STACK_1    4.hex
   cr."    DATA_STACK_2    = "  DATA_STACK_2    4.hex
   cr."    DATA_STACK_3    = "  DATA_STACK_3    4.hex
   cr                                             
   cr ." STACK_MEMORY_START = "  STACK_MEMORY_START 4.hex 
   cr ." TOTAL_STACK_SIZE   = "  TOTAL_STACK_SIZE   4.hex
;

\\
; *****************************************************************************
; Data and Return stack allocation, four pairs of data and return stacks
; Note : the return stack must be in the lowest 64K byte segment, for the BIOS calls to work
; *****************************************************************************
%define DATA_STACK_SIZE_0   $0400   ; 
%define DATA_STACK_SIZE_1   $0500   ; must be > $400 for colorForth "Life" program to work
%define DATA_STACK_SIZE_2   $0100   ; 
%define DATA_STACK_SIZE_3   $0100   ; 
%define DATA_STACK_SIZE_GAP $0100   ; leave space under the last data stack to check for underflow 
      
%define RETURN_STACK_SIZE   $0100   ; 

; return stacks
%define RETURN_STACK_0      $7800       ; top of stack memory area 
%define RETURN_STACK_1      RETURN_STACK_0 - RETURN_STACK_SIZE 
%define RETURN_STACK_2      RETURN_STACK_1 - RETURN_STACK_SIZE 
%define RETURN_STACK_3      RETURN_STACK_2 - RETURN_STACK_SIZE 

; data stacks
%define DATA_STACK_0        RETURN_STACK_3 - RETURN_STACK_SIZE       
%define DATA_STACK_1        DATA_STACK_0   - DATA_STACK_SIZE_0  ; BIG data stack for the show task
%define DATA_STACK_2        DATA_STACK_1   - DATA_STACK_SIZE_1  
%define DATA_STACK_3        DATA_STACK_2   - DATA_STACK_SIZE_2  
%define STACK_MEMORY_START  DATA_STACK_3 - DATA_STACK_SIZE_3 - DATA_STACK_SIZE_GAP 

; four pairs of stacks, one for each task   
RETURN_STACK_0 STACK_MEMORY_START - %define TOTAL_STACK_SIZE 

; *****************************************************************************
; *****************************************************************************
                       

return stacks :
   RETURN_STACK_0  = 7800
   RETURN_STACK_1  = 7700
   RETURN_STACK_2  = 7600
   RETURN_STACK_3  = 7500

data stacks :
   DATA_STACK_0    = 7400
   DATA_STACK_1    = 7000
   DATA_STACK_2    = 6B00
   DATA_STACK_3    = 6A00

STACK_MEMORY_START = 6800
TOTAL_STACK_SIZE   = 1000


; stack allocation, four pairs of data and return stacks
; Note : the return stack must be in the lowest 64K byte segment, for the BIOS calls to work.
%define RETURN_STACK_0      0x7800 ; top of stack memory area ; 0xa0000 in CM's code, 0x10000 in JC's code
%define DATA_STACK_SIZE     0x0200  ; must be > $400 for colorForth "Life" program to work
%define RETURN_STACK_SIZE   0x0200
; combined stack sizes
%define STACK_SIZE          ( DATA_STACK_SIZE + RETURN_STACK_SIZE )
%define TOTAL_STACK_SIZE    ( STACK_SIZE * 4 )  ; four pairs of stacks, one for each task

%define STACK_MEMORY_START  ( RETURN_STACK_0 - TOTAL_STACK_SIZE )
; data stacks
%define DATA_STACK_0        ( RETURN_STACK_0 - RETURN_STACK_SIZE ) ; 0x9f400 in CM's code
%define DATA_STACK_1        ( DATA_STACK_0 - STACK_SIZE )
%define DATA_STACK_2        ( DATA_STACK_1 - STACK_SIZE )
%define DATA_STACK_3        ( DATA_STACK_2 - STACK_SIZE )

; return stacks
%define RETURN_STACK_1      ( RETURN_STACK_0 - STACK_SIZE )
%define RETURN_STACK_2      ( RETURN_STACK_1 - STACK_SIZE )
%define RETURN_STACK_3      ( RETURN_STACK_2 - STACK_SIZE )
