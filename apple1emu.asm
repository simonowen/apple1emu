; Apple 1 emulator for SAM Coupe, by Simon Owen (v1.0)
;
; WWW: http://simonowen.com/sam/apple1emu/

base:          equ  &b000           ; Spare-ish Apple 1 space

status:        equ  249             ; Status and extended keyboard port
lmpr:          equ  250             ; Low Memory Page Register
hmpr:          equ  251             ; High Memory Page Register
vmpr:          equ  252             ; Video Memory Page Register
keyboard:      equ  254             ; Keyboard port
border:        equ  254             ; Border port
rom0_off:      equ  %00100000       ; LMPR bit to disable ROM0
rom1_on:       equ  %01000000       ; LMPR bit to enable ROM1
vmpr_mode2:    equ  %00100000       ; Mode 2 select for VMPR

low_page:      equ  3               ; LMPR during emulation
screen_page:   equ  5               ; SAM display
file_page:     equ  6               ; File import text page

bord_stp:      equ  2               ; STP instruction halted CPU (red)
bord_wai:      equ  6               ; WAI instruction waiting for interrupt (yellow)

m6502_nmi:     equ  &fffa           ; nmi vector address
m6502_reset:   equ  &fffc           ; reset vector address
m6502_int:     equ  &fffe           ; int vector address (also for BRK)

getkey:        equ &1cab            ; SAM ROM key reading (NZ=got key in A, A=0 for no key)


; Apple 1 keyboard and display I/O locations

kbd_data:      equ &d010            ; keyboard data
kbd_ctrl:      equ &d011            ; keyboard control
dsp_data:      equ &d012            ; display data
dsp_ctrl:      equ &d013            ; display control

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               org  base
               dump $
               autoexec

start:         di

               ld   a,low_page+rom0_off
               out  (lmpr),a
               ld   a,vmpr_mode2+screen_page
               out  (vmpr),a
               ld   sp,stack_top

               call reorder_decode  ; optimise instruction decode table
               call set_sam_attrs   ; set the mode 2 attrs so the screen is visible
               call setup_im2       ; enable IM 2

reset_loop:    ld   hl,0
               ld   (dsp_data),hl   ; display ready
               ld   (kbd_ctrl),hl   ; no key available

               ld   hl,(m6502_reset) ; start from reset vector
               ld   (reg_pc),hl

               ei
               call execute         ; GO!
               jr   reset_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Utility functions

; Fill SAM mode 2 display attributes to make the screen visible
set_sam_attrs: ld   a,screen_page+rom0_off
               out  (lmpr),a

               ld   hl,&2000
               ld   bc,&1844        ; 24 blocks of bright green on black
clear_lp:      ld   (hl),c
               inc  l
               jr   nz,clear_lp
               inc  h
               djnz clear_lp

               ld   a,low_page+rom0_off
               out  (lmpr),a
               ret

; Scroll the screen up 1 row and clear the bottom line
scroll_screen: ld   hl,0
               ld   d,h
               ld   e,l
               inc  h
               ld   bc,&1700
scroll_lp:     FOR  32, ldi         ; 32 * LDI
               jp   pe,scroll_lp
               dec  h
scroll_clr:    ld   (hl),0
               inc  l
               jp   nz,scroll_clr
               ret

; Advance the cursor 1 character, wrapping and scrolling if necessary
advance_chr:   ld   hl,(cursor_xy)
               cp   &5f
               jr   z,back_chr
               cp   &0a
               jr   z,advance_line
               cp   &0d
               jr   z,advance_line
               ld   a,l
               add  a,6
               ld   l,a
               cp   &f0
               jr   c,no_wrap
advance_line:  ld   l,0
               inc  h
               ld   a,h
               cp   &18
               jr   nz,no_wrap
               dec  h
               exx
               call scroll_screen
               exx
no_wrap:       ld   (cursor_xy),hl
               ret
back_chr:      ld   a,l
               sub  6
               ld   l,a
               jr   nc,no_wrap
               ld   l,39*6
               ld   a,h
               sub  1
               adc  a,0
               ld   h,a
               jr   no_wrap

; Map display character from SAM to Apple 1
map_chr:       cp   &20
               jr   c,invalid_chr
               and  %10111111
               xor  %01100000
               ret
invalid_chr:   xor  a
               ret

; Display character in A at current cursor position
display_chr:   add  a,a             ; * 2
               add  a,a             ; * 4
               ld   l,a
               ld   h,0
               add  hl,hl           ; * 8
               ld   de,font_data
               add  hl,de
               ld   d,mask_data/256
               exx
               ld   hl,(cursor_xy)
               ld   a,l
               and  %00000111
               srl  l
               srl  l
               srl  l
               inc  l
               exx
               ld   e,a
               exx
               ld   de,&0020
               ld   b,8
draw_lp:       exx
               ld   c,(hl)
               inc  l
               xor  a
               cp   e
               jr   z,no_rot
               ld   b,e
rot_lp:        srl  c
               rra
               djnz rot_lp
no_rot:        ld   b,a
               ld   a,(de)
               inc  e
               exx
               and  (hl)
               inc  l
               exx
               or   c
               ex   af,af'
               ld   a,(de)
               dec  e
               exx
               and  (hl)
               exx
               or   b
               exx
               ld   (hl),a
               dec  l
               ex   af,af'
               ld   (hl),a
               add  hl,de
               djnz draw_lp
               ret

; LSB=pixel position (0-240), MSB=display row (0-23)
cursor_xy:     defw 0

; Map key symbols from SAM to Apple 1
map_key:       cp   &0c
               jr   z,del_key
               cp   &fc
               jr   z,tab_key
               cp   &80
               jr   nc,ignore_key
               cp   &61             ; 'a'
               ret  c
               cp   &7b             ; 'z'+1
               ret  nc
               and  %11011111
               ret
del_key:       ld   a,&5f           ; Delete -> _
               ret
tab_key:       ld   a,&09           ; standard tab
               ret
invalid_key:   ld   a,&20           ; space for invalid
               ret
ignore_key:    xor  a               ; ignore
               ret

; Get a type-in key from the imported file
get_filekey:   ld   a,file_page+rom0_off
               out  (lmpr),a

               ld   hl,(filepos)
               ld   a,h
               or   l
               or   (hl)
               jr   z,file_done     ; jump if no file

file_skip:     ld   a,(hl)          ; fetch next file character
               inc  hl
               and  a
               jr   z,clear_file2   ; clear file at end
               cp   &0d             ; CR?
               jr   z,file_skip     ; ignore CR in file
               cp   &0a             ; LF?
               jr   nz,file_done
               ld   a,&0d           ; convert LF to CR
file_done:     ld   (filepos),hl
               ex   af,af'
               ld   a,screen_page+rom0_off
               out  (lmpr),a
               ex   af,af'
               and  a
               ret
filepos:       defw 0

; Clear the file area to disable type-in
clear_file:    ld   a,file_page+rom0_off
               out  (lmpr),a
clear_file2:   ld   hl,0
               ld   c,l
clr_file_lp:   ld   (hl),c
               inc  l
               jr   nz,clr_file_lp
               inc  h
               bit  7,h
               jr   nz,clr_file_lp
               ld   h,l
               xor  a               ; no file character
               jr   file_done

update_io:
               ld   a,&f7
               in   a,(status)
               and  %00100000
               jr   nz,not_esc      ; jump if Esc not pressed
still_esc:     ld   a,&f7
               in   a,(status)
               and  %00100000
               jr   z,still_esc     ; wait until Esc released

               call clear_file      ; Esc cancels type-in mode

               ld   a,&fe
               in   a,(keyboard)
               rra
               ld   a,&1b           ; Esc character
               jr   c,got_key       ; unshifted gives chr

               ld   a,&c9           ; RET
               ld   (main_loop),a   ; exit to reset at next instruction
               jr   no_key
not_esc:
               ld   a,&7f
               in   a,(keyboard)
               bit  1,a
               jr   nz,not_sym

               ld   a,&f7
               in   a,(keyboard)
               rra
               ld   c,&ff           ; line interrupt disable
               jr   nc,got_line
               rra
               ld   c,88            ; centre of display (312/2-68=88)
               jr   c,not_sym
got_line:      ld   a,c
               out  (status),a      ; set or disable line interrupt
not_sym:
               ld   hl,kbd_ctrl
               bit  7,(hl)          ; key available?
               jr   nz,no_key       ; no need to read more yet

               call get_filekey     ; got a type-in key from file?
               jr   nz,got_key      ; jump if we have

skip_key:      ld   a,&1f+rom1_on
               out  (lmpr),a
               call getkey
               ex   af,af'
               ld   a,screen_page+rom0_off
               out  (lmpr),a
               ex   af,af'
               jr   z,no_key
               call map_key
               and  a
               jr   z,skip_key
got_key:
               ld   hl,kbd_data
               ld   (hl),a
               set  7,(hl)          ; bit 7 always set
               inc  l
               set  7,(hl)          ; key available
no_key:
               ld   hl,dsp_data
               bit  7,(hl)          ; display char available?
               jr   z,no_char
               res  7,(hl)          ; display ready
               ld   a,(hl)          ; fetch character to display
               and  a
               jr   z,done_draw
               cp   &7f
               jr   z,done_draw
               push af
               ld   a,&00           ; space
               call display_chr     ; erase cursor
               pop  af
               cp   &5f             ; backspace? (underscore)
               jr   z,done_draw2    ; if so, no need to process further

               push af
               call map_chr         ; map output from SAM->Apple 1
               call display_chr     ; show it
               pop  af
done_draw2:    call advance_chr     ; advance the cursor position
               ld   a,&3f           ; cursor block
               call display_chr     ; show cursor
done_draw:
no_char:
               ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Interrupt handling

setup_im2:     ld   hl,im2_table
               ld   a,im2_jp/256
im2_fill:      ld   (hl),a
               inc  l
               jr   nz,im2_fill
               inc  h
               ld   (hl),a          ; complete the final entry
               ld   a,im2_table/256
               ld   i,a
               im   2               ; set interrupt mode 2
               ret

im2_handler:   push af
               push bc
               push de
               push hl
               ex   af,af'
               exx
               push af
               push bc
               push de
               push hl
               push ix
               push iy

               in   a,(lmpr)
               push af
               ld   a,screen_page+rom0_off
               out  (lmpr),a

               call update_io

               pop  af
               out  (lmpr),a

               pop  iy
               pop  ix
               pop  hl
               pop  de
               pop  bc
               pop  af
               exx
               ex   af,af'
               pop  hl
               pop  de
               pop  bc
               pop  af
               ei
               reti
end_1:

; IM 2 table must be aligned to 256-byte boundary
               defs -$\256
im2_table:     defs 257

; IM 2 vector must have LSB==MSB
               defs $/256-1
stack_top:                          ; stack fits nicely in the slack space
im2_jp:        jp   im2_handler


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 65C02 emulation

execute:       ld   a,&1a           ; LD A,(DE)
               ld   (main_loop),a
               call load_state
               jr   main_loop

read_write_loop:
write_loop:    ld   a,h
               cp   &d0
               jr   z,io_write

zwrite_loop:
zread_write_loop:
main_loop:     ld   a,(de)          ; fetch opcode
               inc  de              ; PC=PC+1
               ld   l,a
               ld   h,decode_table/256
               ld   a,(hl)          ; handler low
               inc  h
               ld   h,(hl)          ; handler high
               ld   l,a
               jp   (hl)            ; execute!

               ; I/O write
io_write:      ld   a,l
               cp   &12             ; display char?
               jr   nz,main_loop
               set  7,(hl)          ; display busy
               jp   main_loop

read_loop:
zread_loop:    ld   a,h
               cp   &d0
               jr   z,io_read

               ld   a,(de)          ; fetch opcode
               inc  de              ; PC=PC+1
               ld   l,a
               ld   h,decode_table/256
               ld   a,(hl)          ; handler low
               inc  h
               ld   h,(hl)          ; handler high
               ld   l,a
               jp   (hl)            ; execute!

io_read:       ld   a,l
               cp   &10             ; key read?
               jr   nz,main_loop
               inc  l
               res  7,(hl)          ; key not available
               jp   main_loop


; 6502 addressing modes, shared by logical and arithmetic
; instructions, but inlined into the load and store.

a_indirect_x:  ld   a,(de)          ; indirect pre-indexed with X
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               jp   (ix)

a_zero_page:   ld   a,(de)          ; zero-page
               inc  de
               ld   l,a
               ld   h,0
               jp   (ix)

a_absolute:    ex   de,hl           ; absolute (2-bytes)
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               jp   (ix)

a_indirect_y:  ld   a,(de)          ; indirect post-indexed with Y
               inc  de
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               jp   (ix)

a_zero_page_x: ld   a,(de)          ; zero-page indexed with X
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               jp   (ix)

a_zero_page_y: ld   a,(de)          ; zero-page indexed with Y
               inc  de
               defb &fd
               add  a,l             ; add Y (may wrap in zero page)
               ld   l,a
               ld   h,0
               jp   (ix)

a_absolute_y:  ex   de,hl           ; absolute indexed with Y
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               jp   (ix)

a_absolute_x:  ex   de,hl           ; absolute indexed with X
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               jp   (ix)

a_indirect_z:  ld   a,(de)          ; indirect zero-page [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               jp   (ix)

; Instruction implementations

i_nop:         equ  main_loop
i_undoc_1:     equ  main_loop
i_undoc_3:     inc  de              ; 3-byte NOP
i_undoc_2:     inc  de              ; 2-byte NOP
               jp   main_loop

i_clc:         exx                  ; clear carry
               ld   c,0
               exx
               jp   main_loop
i_sec:         exx                  ; set carry
               ld   c,1
               exx
               jp   main_loop
i_cli:         exx                  ; clear interrupt disable
               res  2,d
               exx
               jp   main_loop
i_sei:         exx                  ; set interrupt disable
               set  2,d
               exx
               jp   main_loop
i_clv:         exx                  ; clear overflow
               ld   b,0
               exx
               jp   main_loop
i_cld:         exx                  ; clear decimal mode
               res  3,d
               exx
               xor  a               ; NOP
               ld   (adc_daa),a     ; use binary mode for adc
               ld   (sbc_daa),a     ; use binary mode for sbc
               jp   main_loop
i_sed:         exx
               set  3,d
               exx
               ld   a,&27           ; DAA
               ld   (adc_daa),a     ; use decimal mode for adc
               ld   (sbc_daa),a     ; use decimal mode for sbc
               jp   main_loop

i_bpl:         ld   a,(de)
               inc  de
               ex   af,af'
               ld   l,a             ; copy N
               ex   af,af'
               bit  7,l             ; test N
               jr   z,i_branch      ; branch if plus
               jp   main_loop
i_bmi:         ld   a,(de)
               inc  de
               ex   af,af'
               ld   l,a             ; copy N
               ex   af,af'
               bit  7,l             ; test N
               jr   nz,i_branch     ; branch if minus
               jp   main_loop
i_bvc:         ld   a,(de)          ; V in bit 6
               inc  de              ; V set if non-zero
               exx
               bit  6,b
               exx
               jr   z,i_branch      ; branch if V clear
               jp   main_loop
i_bvs:         ld   a,(de)          ; V in bit 6
               inc  de
               exx
               bit  6,b
               exx
               jr   nz,i_branch     ; branch if V set
               jp   main_loop
i_bcc:         ld   a,(de)          ; C in bit 1
               inc  de
               exx
               bit  0,c
               exx
               jr   z,i_branch      ; branch if C clear
               jp   main_loop
i_bcs:         ld   a,(de)
               inc  de
               exx
               bit  0,c
               exx
               jr   nz,i_branch     ; branch if C set
               jp   main_loop
i_beq:         ld   a,(de)
               inc  de
               inc  c
               dec  c               ; zero?
               jr   z,i_branch      ; branch if zero
               jp   main_loop
i_bne:         ld   a,(de)
               inc  de
               inc  c
               dec  c               ; zero?
               jp   z,main_loop     ; no branch if not zero
i_branch:      ld   l,a             ; offset low
               rla                  ; set carry with sign
               sbc  a,a             ; form high byte for offset
               ld   h,a
               add  hl,de           ; PC=PC+e
               ex   de,hl
               jp   main_loop
i_bra:         ld   a,(de)          ; unconditional branch [65C02]
               inc  de
               jr   i_branch

i_bbr_0:       ld   a,%00000001     ; BBRn [65C02]
               jp   i_bbs
i_bbr_1:       ld   a,%00000010
               jp   i_bbs
i_bbr_2:       ld   a,%00000100
               jp   i_bbs
i_bbr_3:       ld   a,%00001000
               jp   i_bbs
i_bbr_4:       ld   a,%00010000
               jp   i_bbs
i_bbr_5:       ld   a,%00100000
               jp   i_bbs
i_bbr_6:       ld   a,%01000000
               jp   i_bbs
i_bbr_7:       ld   a,%10000000
i_bbr:         ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,0
               ex   de,hl
               and  (hl)
               ld   a,(de)
               inc  de
               jr   z,i_branch      ; ToDo: read_loop after branch
               jp   read_loop

i_bbs_0:       ld   a,%00000001     ; BBSn [65C02]
               jp   i_bbs
i_bbs_1:       ld   a,%00000010
               jp   i_bbs
i_bbs_2:       ld   a,%00000100
               jp   i_bbs
i_bbs_3:       ld   a,%00001000
               jp   i_bbs
i_bbs_4:       ld   a,%00010000
               jp   i_bbs
i_bbs_5:       ld   a,%00100000
               jp   i_bbs
i_bbs_6:       ld   a,%01000000
               jp   i_bbs
i_bbs_7:       ld   a,%10000000
i_bbs:         ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,0
               ex   de,hl
               and  (hl)
               ld   a,(de)
               inc  de
               jr   nz,i_branch     ; ToDo: read_loop after branch
               jp   read_loop

i_jmp_a:       ex   de,hl           ; JMP nn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               jp   main_loop

i_jmp_i:       ex   de,hl           ; JMP (nn)
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   e,(hl)
;              inc  l               ; 6502 bug wraps within page, *OR*
               inc  hl              ; 65C02 spans pages correctly
               ld   d,(hl)
               jp   main_loop

i_jmp_ax:      ex   de,hl           ; JMP (nn,X) [65C02]
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)          ; carry spans page
               ld   d,a
               inc  hl
               ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               jp   main_loop

i_jsr:         ex   de,hl           ; JSR nn
               ld   e,(hl)          ; subroutine low
               inc  hl              ; only 1 inc - we push ret-1
               ld   d,(hl)          ; subroutine high
               ld   a,h             ; PCh
               exx
               ld   (hl),a          ; push ret-1 high byte
               dec  l               ; S--
               exx
               ld   a,l             ; PCl
               exx
               ld   (hl),a          ; push ret-1 low byte
               dec  l               ; S--
               exx
               jp   main_loop

i_brk:         inc  de              ; return to BRK+2
               ld   a,d
               exx
               ld   (hl),a          ; push return MSB
               dec  l               ; S--
               exx
               ld   a,e
               exx
               ld   (hl),a          ; push return LSB
               dec  l               ; S--
               ld   a,d
               or   %00010000       ; set B flag (temp)
               ld   (hl),a          ; push flags with B set
               dec  l               ; S--
               set  2,d             ; set I flag
               exx
               ld   de,(m6502_int)  ; fetch interrupt handler
               jp   main_loop

i_rts:         exx                  ; RTS
               inc  l               ; S++
               ld   a,(hl)          ; PC LSB
               exx
               ld   e,a
               exx
               inc  l               ; S++
               ld   a,(hl)          ; PC MSB
               exx
               ld   d,a
               inc  de              ; PC++ (strange but true)
               jp   main_loop

i_rti:         exx                  ; RTI
               inc  l               ; S++
               ld   a,(hl)          ; pop P
               or   %00110000       ; set T and B flags
               call split_p_exx     ; split P into status+flags (already exx)
               exx
               inc  l               ; S++
               ld   a,(hl)          ; pop return LSB
               exx
               ld   e,a
               exx
               inc  l               ; S++
               ld   a,(hl)          ; pop return MSB
               exx
               ld   d,a
               jp   main_loop

i_php:         call make_p          ; make P from status+flags
               or   %00010000       ; B always pushed as 1
               exx
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   main_loop
i_plp:         exx                  ; PLP
               inc  l               ; S++
               ld   a,(hl)          ; P
               or   %00110000       ; set T and B flags
               exx
               call split_p         ; split P into status+flags
               jp   main_loop
i_pha:         ld   a,b             ; PHA
               exx
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   main_loop
i_pla:         exx                  ; PLA
               inc  l               ; S++
               ld   a,(hl)
               exx
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_phx:         defb &fd             ; PHX [65C02]
               ld   a,h             ; X
               exx
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   main_loop
i_plx:         exx                  ; PLX [65C02]
               inc  l               ; S++
               ld   a,(hl)
               exx
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_phy:         defb &fd             ; PHY [65C02]
               ld   a,l             ; Y
               exx
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   main_loop
i_ply:         exx                  ; PLY [65C02]
               inc  l               ; S++
               ld   a,(hl)
               exx
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_dex:         defb &fd             ; DEX
               dec  h               ; X--
               defb &fd
               ld   a,h             ; X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_dey:         defb &fd             ; DEY
               dec  l               ; Y--
               defb &fd
               ld   a,l             ; Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_inx:         defb &fd             ; INX
               inc  h               ; X++
               defb &fd
               ld   a,h             ; X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_iny:         defb &fd             ; INY
               inc  l               ; Y++
               defb &fd
               ld   a,l             ; Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_txa:         defb &fd             ; TXA
               ld   a,h             ; X
               ld   b,a             ; A=X
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_tya:         defb &fd             ; TYA
               ld   a,l             ; Y
               ld   b,a             ; A=Y
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_tax:         defb &fd             ; TAX
               ld   h,b             ; X=A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop
i_tay:         defb &fd             ; TAY
               ld   l,b             ; Y=A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop
i_txs:         defb &fd             ; TXS
               ld   a,h             ; X
               exx
               ld   l,a             ; set S (no flags set)
               exx
               jp   main_loop
i_tsx:         exx                  ; TSX
               ld   a,l             ; fetch S
               exx
               defb &fd
               ld   h,a             ; X=S
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop


; For speed, LDA/LDX/LDY instructions have addressing inlined

i_lda_ix:      ld   a,(de)          ; LDA ($nn,X)
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   b,(hl)          ; set A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   zread_loop
i_lda_z:       ld   a,(de)          ; LDA $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   b,(hl)          ; set A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   zread_loop
i_lda_a:       ex   de,hl           ; LDA $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   b,(hl)          ; set A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   read_loop
i_lda_iy:      ld   a,(de)          ; LDA ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   b,(hl)          ; set A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   read_loop
i_lda_zx:      ld   a,(de)          ; LDA $nn,X
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   b,(hl)          ; set A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   zread_loop
i_lda_ay:      ex   de,hl           ; LDA $nnnn,Y
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   b,(hl)          ; set A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   read_loop
i_lda_ax:      ex   de,hl           ; LDA $nnnn,X
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   b,(hl)          ; set A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   read_loop
i_lda_i:       ld   a,(de)          ; LDA #$nn
               inc  de
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_lda_iz:      ld   a,(de)          ; LDA ($nn) [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   b,(hl)          ; set A
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   read_loop

i_ldx_z:       ld   a,(de)          ; LDX $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   zread_loop
i_ldx_a:       ex   de,hl           ; LDX $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,(hl)
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   read_loop
i_ldx_zy:      ld   a,(de)          ; LDX $nn,Y
               inc  de
               defb &fd
               add  a,l             ; add Y (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   zread_loop
i_ldx_ay:      ex   de,hl           ; LDX $nnnn,Y
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   a,(hl)
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   read_loop
i_ldx_i:       ld   a,(de)          ; LDX #$nn
               inc  de
               defb &fd
               ld   h,a             ; set X
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop

i_ldy_z:       ld   a,(de)          ; LDY $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   zread_loop
i_ldy_a:       ex   de,hl           ; LDY $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,(hl)
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   read_loop
i_ldy_zx:      ld   a,(de)          ; LDY $nn,X
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   zread_loop
i_ldy_ax:      ex   de,hl           ; LDY $nnnn,X
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   a,(hl)
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   read_loop
i_ldy_i:       ld   a,(de)          ; LDY #$nn
               inc  de
               defb &fd
               ld   l,a             ; set Y
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   main_loop


; For speed, STA/STX/STY instructions have addressing inlined

i_sta_ix:      ld   a,(de)          ; STA ($xx,X)
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   (hl),b
               jp   zwrite_loop
i_sta_z:       ld   a,(de)          ; STA $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   (hl),b
               jp   zwrite_loop
i_sta_iy:      ld   a,(de)
               inc  de
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               inc  l
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   (hl),b
               jp   write_loop
i_sta_zx:      ld   a,(de)
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   (hl),b
               jp   zwrite_loop
i_sta_ay:      ex   de,hl
               defb &fd
               ld   a,l             ; Y
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   (hl),b
               jp   write_loop

i_sta_ax:      ex   de,hl
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   (hl),b
               jp   write_loop
i_sta_a:       ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   (hl),b
               jp   write_loop
i_sta_iz:      ld   a,(de)          ; STA ($nn) [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   (hl),b          ; store A
               jp   write_loop

i_stx_z:       ld   a,(de)
               inc  de
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,h             ; X
               ld   (hl),a
               jp   zwrite_loop
i_stx_zy:      ld   a,(de)
               inc  de
               defb &fd
               add  a,l             ; add Y (may wrap in zero page)
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,h             ; X
               ld   (hl),a
               jp   zwrite_loop
i_stx_a:       ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               defb &fd
               ld   a,h             ; X
               ld   (hl),a
               jp   write_loop

i_sty_z:       ld   a,(de)
               inc  de
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               ld   (hl),a
               jp   zwrite_loop
i_sty_zx:      ld   a,(de)
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               defb &fd
               ld   a,l             ; Y
               ld   (hl),a
               jp   zwrite_loop
i_sty_a:       ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               defb &fd
               ld   a,l             ; Y
               ld   (hl),a
               jp   write_loop

i_stz_z:       ld   a,(de)          ; STZ $nn [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   (hl),h
               jp   zwrite_loop
i_stz_zx:      ld   a,(de)
               inc  de
               defb &fd
               add  a,h             ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   (hl),h
               jp   zwrite_loop
i_stz_ax:      ex   de,hl
               defb &fd
               ld   a,h             ; X
               add  a,(hl)
               ld   e,a
               inc  hl
               ld   a,0
               adc  a,(hl)
               ld   d,a
               inc  hl
               ex   de,hl
               ld   (hl),0
               jp   write_loop
i_stz_a:       ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   (hl),0
               jp   write_loop

i_adc_ix:      ld   ix,i_adc
               jp   a_indirect_x
i_adc_z:       ld   ix,i_adc
               jp   a_zero_page
i_adc_a:       ld   ix,i_adc
               jp   a_absolute
i_adc_zx:      ld   ix,i_adc
               jp   a_zero_page_x
i_adc_ay:      ld   ix,i_adc
               jp   a_absolute_y
i_adc_ax:      ld   ix,i_adc
               jp   a_absolute_x
i_adc_iy:      ld   ix,i_adc
               jp   a_indirect_y
i_adc_iz:      ld   ix,i_adc        ; [65C02]
               jp   a_indirect_z
i_adc_i:       ld   h,d
               ld   l,e
               inc  de
i_adc:         exx
               ld   a,c             ; C
               exx
               rra                  ; set up carry
               ld   a,b             ; A
               adc  a,(hl)          ; A+M+C
adc_daa:       nop
               ld   b,a             ; set A
;              jp   set_nvzc
               ; fall through to set_nvzc...

set_nvzc:      ld   c,a             ; set Z
               rla                  ; C in bit 0, no effect on V
               exx
               ld   c,a             ; set C
               jp   pe,set_v
               ld   b,%00000000     ; V clear
               exx
               ld   a,c
               ex   af,af'          ; set N
               jp   read_loop
set_v:         ld   b,%01000000     ; V set
               exx
               ld   a,c
               ex   af,af'          ; set N
               jp   read_loop

i_sbc_ix:      ld   ix,i_sbc
               jp   a_indirect_x
i_sbc_z:       ld   ix,i_sbc
               jp   a_zero_page
i_sbc_a:       ld   ix,i_sbc
               jp   a_absolute
i_sbc_zx:      ld   ix,i_sbc
               jp   a_zero_page_x
i_sbc_ay:      ld   ix,i_sbc
               jp   a_absolute_y
i_sbc_ax:      ld   ix,i_sbc
               jp   a_absolute_x
i_sbc_iy:      ld   ix,i_sbc
               jp   a_indirect_y
i_sbc_iz:      ld   ix,i_sbc        ; [65C02]
               jp   a_indirect_z
i_sbc_i:       ld   h,d
               ld   l,e
               inc  de
i_sbc:         exx
               ld   a,c             ; C
               exx
               rra                  ; set up carry
               ld   a,b             ; A
               ccf                  ; uses inverted carry
               sbc  a,(hl)          ; A-M-(1-C)
sbc_daa:       nop
               ccf                  ; no carry for overflow
               ld   b,a             ; set A
               jp   set_nvzc

i_and_ix:      ld   ix,i_and
               jp   a_indirect_x
i_and_z:       ld   ix,i_and
               jp   a_zero_page
i_and_a:       ld   ix,i_and
               jp   a_absolute
i_and_zx:      ld   ix,i_and
               jp   a_zero_page_x
i_and_ay:      ld   ix,i_and
               jp   a_absolute_y
i_and_ax:      ld   ix,i_and
               jp   a_absolute_x
i_and_iy:      ld   ix,i_and
               jp   a_indirect_y
i_and_iz:      ld   ix,i_and        ; [65C02]
               jp   a_indirect_z
i_and_i:       ld   h,d
               ld   l,e
               inc  de
i_and:         ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   read_loop

i_eor_ix:      ld   ix,i_eor
               jp   a_indirect_x
i_eor_z:       ld   ix,i_eor
               jp   a_zero_page
i_eor_a:       ld   ix,i_eor
               jp   a_absolute
i_eor_zx:      ld   ix,i_eor
               jp   a_zero_page_x
i_eor_ay:      ld   ix,i_eor
               jp   a_absolute_y
i_eor_ax:      ld   ix,i_eor
               jp   a_absolute_x
i_eor_iy:      ld   ix,i_eor
               jp   a_indirect_y
i_eor_iz:      ld   ix,i_eor        ; [65C02]
               jp   a_indirect_z
i_eor_i:       ld   h,d
               ld   l,e
               inc  de
i_eor:         ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   read_loop

i_ora_ix:      ld   ix,i_ora
               jp   a_indirect_x
i_ora_z:       ld   ix,i_ora
               jp   a_zero_page
i_ora_a:       ld   ix,i_ora
               jp   a_absolute
i_ora_zx:      ld   ix,i_ora
               jp   a_zero_page_x
i_ora_ay:      ld   ix,i_ora
               jp   a_absolute_y
i_ora_ax:      ld   ix,i_ora
               jp   a_absolute_x
i_ora_iy:      ld   ix,i_ora
               jp   a_indirect_y
i_ora_iz:      ld   ix,i_ora        ; [65C02]
               jp   a_indirect_z
i_ora_i:       ld   h,d
               ld   l,e
               inc  de
i_ora:         ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   read_loop

i_cmp_ix:      ld   ix,i_cmp
               jp   a_indirect_x
i_cmp_z:       ld   ix,i_cmp
               jp   a_zero_page
i_cmp_a:       ld   ix,i_cmp
               jp   a_absolute
i_cmp_zx:      ld   ix,i_cmp
               jp   a_zero_page_x
i_cmp_ay:      ld   ix,i_cmp
               jp   a_absolute_y
i_cmp_ax:      ld   ix,i_cmp
               jp   a_absolute_x
i_cmp_iy:      ld   ix,i_cmp
               jp   a_indirect_y
i_cmp_iz:      ld   ix,i_cmp        ; [65C02]
               jp   a_indirect_z
i_cmp_i:       ld   h,d
               ld   l,e
               inc  de
i_cmp:         ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ccf
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   read_loop

i_cpx_z:       ld   ix,i_cpx
               jp   a_zero_page
i_cpx_a:       ld   ix,i_cpx
               jp   a_absolute
i_cpx_i:       ld   h,d
               ld   l,e
               inc  de
i_cpx:         defb &fd
               ld   a,h             ; X
               sub  (hl)            ; X-x (result discarded)
               ccf
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   read_loop

i_cpy_z:       ld   ix,i_cpy
               jp   a_zero_page
i_cpy_a:       ld   ix,i_cpy
               jp   a_absolute
i_cpy_i:       ld   h,d
               ld   l,e
               inc  de
i_cpy:         defb &fd
               ld   a,l             ; Y
               sub  (hl)            ; Y-x (result discarded)
               ccf
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   read_loop


i_dec_z:       ld   ix,i_dec_zp
               jp   a_zero_page
i_dec_zx:      ld   ix,i_dec_zp
               jp   a_zero_page_x
i_dec_a:       ld   ix,i_dec
               jp   a_absolute
i_dec_ax:      ld   ix,i_dec
               jp   a_absolute_x
i_dec:         dec  (hl)            ; mem--
               ld   c,(hl)          ; set Z
               ld   a,c
               ex   af,af'          ; set N
               jp   read_write_loop
i_dec_zp:      dec  (hl)            ; zero-page--
               ld   c,(hl)          ; set Z
               ld   a,c
               ex   af,af'          ; set N
               jp   zread_write_loop
i_dec_ac:      dec  b               ; A-- [65C02]
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop

i_inc_z:       ld   ix,i_inc_zp
               jp   a_zero_page
i_inc_zx:      ld   ix,i_inc_zp
               jp   a_zero_page_x
i_inc_a:       ld   ix,i_inc
               jp   a_absolute
i_inc_ax:      ld   ix,i_inc
               jp   a_absolute_x
i_inc:         inc  (hl)            ; mem++
               ld   c,(hl)          ; set Z
               ld   a,c
               ex   af,af'          ; set N
               jp   read_write_loop
i_inc_zp:      inc  (hl)            ; zero-page++
               ld   c,(hl)          ; set Z
               ld   a,c
               ex   af,af'          ; set N
               jp   zread_write_loop
i_inc_ac:      inc  b               ; A++ [65C02]
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop

i_asl_z:       ld   ix,i_asl
               jp   a_zero_page
i_asl_zx:      ld   ix,i_asl
               jp   a_zero_page_x
i_asl_a:       ld   ix,i_asl
               jp   a_absolute
i_asl_ax:      ld   ix,i_asl
               jp   a_absolute_x
i_asl_acc:     sla  b               ; A << 1
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop
i_asl:         ld   a,(hl)          ; x
               add  a,a             ; x << 1
               ld   (hl),a          ; set memory
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop

i_lsr_z:       ld   ix,i_lsr
               jp   a_zero_page
i_lsr_zx:      ld   ix,i_lsr
               jp   a_zero_page_x
i_lsr_a:       ld   ix,i_lsr
               jp   a_absolute
i_lsr_ax:      ld   ix,i_lsr
               jp   a_absolute_x
i_lsr_acc:     srl  b               ; A >> 1
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,b             ; set Z
               ld   a,b
               ex   af,af'          ; set N
               jp   main_loop
i_lsr:         ld   a,(hl)          ; x
               srl  a               ; x >> 1
               ld   (hl),a          ; set memory
               exx
               rl   c               ; retrieve carry
               exx
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop

i_rol_z:       ld   ix,i_rol
               jp   a_zero_page
i_rol_zx:      ld   ix,i_rol
               jp   a_zero_page_x
i_rol_a:       ld   ix,i_rol
               jp   a_absolute
i_rol_ax:      ld   ix,i_rol
               jp   a_absolute_x
i_rol_acc:     ld   a,b
               exx
               rr   c               ; set up carry
               rla                  ; A << 1
               rl   c               ; retrieve carry
               exx
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_rol:         ld   a,(hl)          ; x
               exx
               rr   c               ; set up carry
               rla                  ; x << 1
               rl   c               ; retrieve carry
               exx
               ld   (hl),a          ; set memory
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop

i_ror_z:       ld   ix,i_ror
               jp   a_zero_page
i_ror_zx:      ld   ix,i_ror
               jp   a_zero_page_x
i_ror_a:       ld   ix,i_ror
               jp   a_absolute
i_ror_ax:      ld   ix,i_ror
               jp   a_absolute_x
i_ror_acc:     ld   a,b
               exx
               rr   c               ; set up carry
               rra                  ; A >> 1
               rl   c               ; retrieve carry
               exx
               ld   b,a             ; set A
               ld   c,b             ; set Z
               ex   af,af'          ; set N
               jp   main_loop
i_ror:         ld   a,(hl)          ; x
               exx
               rr   c               ; set up carry
               rra                  ; x >> 1
               rl   c               ; retrieve carry
               exx
               ld   (hl),a          ; set memory
               ld   c,a             ; set Z
               ex   af,af'          ; set N
               jp   write_loop


i_bit_z:       ld   ix,i_bit
               jp   a_zero_page
i_bit_zx:      ld   ix,i_bit
               jp   a_zero_page_x
i_bit_a:       ld   ix,i_bit
               jp   a_absolute
i_bit_ax:      ld   ix,i_bit
               jp   a_absolute_x
i_bit_i:       ld   h,d             ; BIT #$nn
               ld   l,e
               inc  de
i_bit:         ld   c,(hl)          ; x
               ld   a,c
               ex   af,af'          ; set N
               ld   a,c
               and  %01000000       ; V flag set from bit 6
               exx
               ld   b,a             ; set V
               exx
               ld   a,b             ; A
               and  c               ; perform BIT test
               ld   c,a             ; set Z
               jp   read_loop

i_tsb_z:       ld   ix,i_tsb        ; TSB [65C02]
               jp   a_zero_page
i_tsb_a:       ld   ix,i_tsb
               jp   a_absolute
i_tsb:         ld   c,(hl)          ; x
               ld   a,c
               or   b               ; set bits from A
               ld   (hl),a
               ld   a,c
               and  b               ; test bits against A
               ld   c,a             ; set Z
               jp   write_loop

i_trb_z:       ld   ix,i_trb        ; TRB [65C02]
               jp   a_zero_page
i_trb_a:       ld   ix,i_trb
               jp   a_absolute
i_trb:         ld   c,(hl)          ; x
               ld   a,b             ; A
               cpl                  ; ~A
               and  c               ; reset bits from A
               ld   (hl),a
               ld   a,c
               and  b               ; test bits against A
               ld   c,a             ; set Z
               jp   write_loop

i_smb_0:       ld   a,%00000001     ; SMBn [65C02]
               jp   i_smb
i_smb_1:       ld   a,%00000010
               jp   i_smb
i_smb_2:       ld   a,%00000100
               jp   i_smb
i_smb_3:       ld   a,%00001000
               jp   i_smb
i_smb_4:       ld   a,%00010000
               jp   i_smb
i_smb_5:       ld   a,%00100000
               jp   i_smb
i_smb_6:       ld   a,%01000000
               jp   i_smb
i_smb_7:       ld   a,%10000000
i_smb:         ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,0
               ex   de,hl
               or   (hl)
               ld   (hl),a
               jp   zwrite_loop

i_rmb_0:       ld   a,%11111110     ; RMBn [65C02]
               jp   i_smb
i_rmb_1:       ld   a,%11111101
               jp   i_smb
i_rmb_2:       ld   a,%11111011
               jp   i_smb
i_rmb_3:       ld   a,%11110111
               jp   i_smb
i_rmb_4:       ld   a,%11101111
               jp   i_smb
i_rmb_5:       ld   a,%11011111
               jp   i_smb
i_rmb_6:       ld   a,%10111111
               jp   i_smb
i_rmb_7:       ld   a,%01111111
i_rmb:         ex   de,hl
               ld   e,(hl)
               inc  hl
               ld   d,0
               ex   de,hl
               and  (hl)
               ld   (hl),a
               jp   zwrite_loop

i_stp:         dec  de              ; STP [65C02]
               ld   a,bord_stp
               out  (border),a
               jp   main_loop

i_wai:         dec  de              ; WAI [65C02]
               ld   a,bord_wai
               out  (border),a
               jp   main_loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

make_p:        ex   af,af'
               and  %10000000       ; keep N
               ld   l,a             ; N
               ex   af,af'
               ld   a,c             ; Z
               sub  1               ; set carry if zero
               rla
               rla
               and  %00000010       ; keep 6510 Z bit
               or   l               ; N+Z
               exx
               or   b               ; N+V+Z
               ld   e,a
               ld   a,c
               and  %00000001       ; keep C
               or   e               ; N+V+Z+C
               exx
               ret

split_p:       exx
split_p_exx:   ld   e,a             ; save P
               and  %00111100       ; keep CPU bits
               ld   d,a             ; set status
               ld   a,e
               ex   af,af'          ; set N
               ld   a,e
               and  %01000000       ; keep V
               ld   b,a             ; set V
               ld   a,e
               and  %00000001       ; keep C
               ld   c,a             ; set C
               ld   a,e
               cpl
               and  %00000010       ; Z=0 NZ=2
               exx
               ld   c,a             ; set NZ
               ret

load_state:    ld   a,(reg_a)
               ld   b,a             ; set A
               ld   a,(reg_x)
               defb &fd
               ld   h,a             ; set X to IYh
               ld   a,(reg_y)
               defb &fd
               ld   l,a             ; set Y to IYl
               exx
               ld   a,(reg_s)
               ld   l,a             ; set S
               ld   h,&01           ; MSB for stack pointer
               exx
               ld   a,(reg_p)
               call split_p         ; set P and flags
               ld   de,(reg_pc)     ; set PC
               ret

save_state:    ld   a,b             ; get A
               ld   (reg_a),a
               defb &fd
               ld   a,h             ; get X from IYh
               ld   (reg_x),a
               defb &fd
               ld   a,l             ; get Y from IYl
               ld   (reg_y),a
               exx
               ld   a,l             ; get S
               ld   (reg_s),a
               exx
               call make_p          ; get P
               ld   (reg_pc),de
               ret

; While running we have the 6502 registers in Z80 registers
; These are used only to hold the state before/afterwards
reg_a:         defb 0
reg_p:         defb 0
reg_x:         defb 0
reg_y:         defb 0
reg_s:         defb 0
reg_pc:        defw 0


; Reordering the decode table to group low and high bytes means
; we avoid any 16-bit arithmetic for the decode stage, saving
; 12T on the old method (cool tip from Dave Laundon)

reorder_256:   equ  im2_table

reorder_decode:ld   hl,decode_table
               ld   d,h
               ld   e,l
               ld   bc,reorder_256  ; 256-byte temporary store
reorder_lp:    ld   a,(hl)          ; low byte
               ld   (de),a
               inc  l
               inc  e
               ld   a,(hl)          ; high byte
               ld   (bc),a
               inc  hl
               inc  c
               jr   nz,reorder_lp
               dec  h               ; back to 2nd half (high bytes)
reorder_lp2:   ld   a,(bc)
               ld   (hl),a
               inc  c
               inc  l
               jr   nz,reorder_lp2
               ld   a,&c9           ; RET
               ld   (reorder_decode),A
               ret

               defs -$\256          ; align table to 256-byte boundary

decode_table:  DEFW i_brk,i_ora_ix,i_undoc_1,i_undoc_2     ; 00
               DEFW i_tsb_z,i_ora_z,i_asl_z,i_rmb_0        ; 04
               DEFW i_php,i_ora_i,i_asl_acc,i_undoc_2      ; 08
               DEFW i_tsb_a,i_ora_a,i_asl_a,i_bbr_0        ; 0C

               DEFW i_bpl,i_ora_iy,i_ora_iz,i_undoc_2      ; 10
               DEFW i_trb_z,i_ora_zx,i_asl_zx,i_rmb_1      ; 14
               DEFW i_clc,i_ora_ay,i_inc_ac,i_undoc_3      ; 18
               DEFW i_trb_a,i_ora_ax,i_asl_ax,i_bbr_1      ; 1C

               DEFW i_jsr,i_and_ix,i_undoc_1,i_undoc_2     ; 20
               DEFW i_bit_z,i_and_z,i_rol_z,i_rmb_2        ; 24
               DEFW i_plp,i_and_i,i_rol_acc,i_undoc_2      ; 28
               DEFW i_bit_a,i_and_a,i_rol_a,i_bbr_2        ; 2C

               DEFW i_bmi,i_and_iy,i_and_iz,i_undoc_2      ; 30
               DEFW i_bit_zx,i_and_zx,i_rol_zx,i_rmb_3     ; 34
               DEFW i_sec,i_and_ay,i_dec_ac,i_undoc_3      ; 38
               DEFW i_bit_ax,i_and_ax,i_rol_ax,i_bbr_3     ; 3C

               DEFW i_rti,i_eor_ix,i_undoc_1,i_undoc_2     ; 40
               DEFW i_undoc_2,i_eor_z,i_lsr_z,i_rmb_4      ; 44
               DEFW i_pha,i_eor_i,i_lsr_acc,i_undoc_2      ; 48
               DEFW i_jmp_a,i_eor_a,i_lsr_a,i_bbr_4        ; 4C

               DEFW i_bvc,i_eor_iy,i_eor_iz,i_undoc_2      ; 50
               DEFW i_undoc_2,i_eor_zx,i_lsr_zx,i_rmb_5    ; 54
               DEFW i_cli,i_eor_ay,i_phy,i_undoc_3         ; 58
               DEFW i_undoc_3,i_eor_ax,i_lsr_ax,i_bbr_5    ; 5C

               DEFW i_rts,i_adc_ix,i_undoc_1,i_undoc_2     ; 60
               DEFW i_stz_z,i_adc_z,i_ror_z,i_rmb_6        ; 64
               DEFW i_pla,i_adc_i,i_ror_acc,i_undoc_2      ; 68
               DEFW i_jmp_i,i_adc_a,i_ror_a,i_bbr_6        ; 6C

               DEFW i_bvs,i_adc_iy,i_adc_iz,i_undoc_2      ; 70
               DEFW i_stz_zx,i_adc_zx,i_ror_zx,i_rmb_7     ; 74
               DEFW i_sei,i_adc_ay,i_ply,i_undoc_3         ; 78
               DEFW i_jmp_ax,i_adc_ax,i_ror_ax,i_bbr_7     ; 7C

               DEFW i_bra,i_sta_ix,i_undoc_2,i_undoc_2     ; 80
               DEFW i_sty_z,i_sta_z,i_stx_z,i_smb_0        ; 84
               DEFW i_dey,i_bit_i,i_txa,i_undoc_2          ; 88
               DEFW i_sty_a,i_sta_a,i_stx_a,i_bbs_0        ; 8C

               DEFW i_bcc,i_sta_iy,i_sta_iz,i_undoc_2      ; 90
               DEFW i_sty_zx,i_sta_zx,i_stx_zy,i_smb_1     ; 94
               DEFW i_tya,i_sta_ay,i_txs,i_undoc_2         ; 98
               DEFW i_stz_a,i_sta_ax,i_stz_ax,i_bbs_1      ; 9C

               DEFW i_ldy_i,i_lda_ix,i_ldx_i,i_undoc_2     ; A0
               DEFW i_ldy_z,i_lda_z,i_ldx_z,i_smb_2        ; A4
               DEFW i_tay,i_lda_i,i_tax,i_undoc_2          ; A8
               DEFW i_ldy_a,i_lda_a,i_ldx_a,i_bbs_2        ; AC

               DEFW i_bcs,i_lda_iy,i_lda_iz,i_undoc_2      ; B0
               DEFW i_ldy_zx,i_lda_zx,i_ldx_zy,i_smb_3     ; B4
               DEFW i_clv,i_lda_ay,i_tsx,i_undoc_3         ; B8
               DEFW i_ldy_ax,i_lda_ax,i_ldx_ay,i_bbs_3     ; BC

               DEFW i_cpy_i,i_cmp_ix,i_undoc_2,i_undoc_2   ; C0
               DEFW i_cpy_z,i_cmp_z,i_dec_z,i_smb_4        ; C4
               DEFW i_iny,i_cmp_i,i_dex,i_wai              ; C8
               DEFW i_cpy_a,i_cmp_a,i_dec_a,i_bbs_4        ; CC

               DEFW i_bne,i_cmp_iy,i_cmp_iz,i_undoc_2      ; D0
               DEFW i_undoc_2,i_cmp_zx,i_dec_zx,i_smb_5    ; D4
               DEFW i_cld,i_cmp_ay,i_phx,i_stp             ; D8
               DEFW i_undoc_3,i_cmp_ax,i_dec_ax,i_bbs_5    ; DC

               DEFW i_cpx_i,i_sbc_ix,i_undoc_2,i_undoc_2   ; E0
               DEFW i_cpx_z,i_sbc_z,i_inc_z,i_smb_6        ; E4
               DEFW i_inx,i_sbc_i,i_nop,i_undoc_2          ; E8
               DEFW i_cpx_a,i_sbc_a,i_inc_a,i_bbs_6        ; EC

               DEFW i_beq,i_sbc_iy,i_sbc_iz,i_undoc_2      ; F0
               DEFW i_undoc_2,i_sbc_zx,i_inc_zx,i_smb_7    ; F4
               DEFW i_sed,i_sbc_ay,i_plx,i_undoc_3         ; F8
               DEFW i_undoc_3,i_sbc_ax,i_inc_ax,i_bbs_7    ; FC

font_data:
MDAT "font.bin"

mask_data:     defb %00000011,%11111111
               defb %11000000,%11111111
               defb %11110000,%00111111
               defb %11111100,%00001111

end:           equ  $
length:        equ  end-start


; Ken Wessen's custom BASIC+Krusader+Monitor ROM (&e000-&ffff)
; BRK handler points to mini-monitor in this version
    dump &e000
MDAT "65C02.rom.bin"

; Original Monitor ROM (&ff00-&ffff)
; If uncommented, this will replace the monitor ROM section from above
    dump &ff00
MDAT "apple1.rom"
