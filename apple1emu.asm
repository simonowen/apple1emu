; Apple 1 emulator for SAM Coupe, by Simon Owen
;
; Version 1.2 (5/9/2008)
;
; WWW: http://simonowen.com/sam/apple1emu/

base:          equ  &a000           ; Spare-ish Apple 1 space

status:        equ  &f9             ; Status and extended keyboard port
lmpr:          equ  &fa             ; Low Memory Page Register
hmpr:          equ  &fb             ; High Memory Page Register
vmpr:          equ  &fc             ; Video Memory Page Register
keyboard:      equ  &fe             ; Keyboard port
border:        equ  &fe             ; Border port

rom0_off:      equ  %00100000       ; LMPR bit to disable ROM0
rom1_on:       equ  %01000000       ; LMPR bit to enable ROM1
vmpr_mode2:    equ  %00100000       ; Mode 2 select for VMPR

low_page:      equ  3               ; LMPR during emulation
screen_page:   equ  5               ; SAM display
file_page:     equ  6               ; File import text page

m6502_nmi:     equ  &fffa           ; nmi vector address
m6502_reset:   equ  &fffc           ; reset vector address
m6502_int:     equ  &fffe           ; int vector address (also for BRK)

getkey:        equ  &1cab           ; SAM ROM key reading (NZ=got key in A, A=0 for no key)


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

               call set_sam_attrs   ; set the mode 2 attrs so the screen is visible
               call setup_im2       ; enable IM 2

reset_loop:    ld   hl,0
               ld   (dsp_data),hl   ; display ready
               ld   (kbd_data),hl   ; no key available

               ld   hl,(m6502_reset) ; start from reset vector
               ld   (reg_pc),hl
               ld   a,&04           ; interrupts disabled
               ld   (reg_p),a

               ei
               call load_state
               call execute         ; GO!
               call save_state
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
map_chr:       sub  &20
               ret  nc
               xor  a
               ret

; Display character in A at current cursor position
display_chr:   add  a,a             ; * 2
               ld   l,a
               ld   h,0
               add  hl,hl           ; * 4
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

               call clear_file      ; Esc cancels type-in mode

               ld   a,&fe
               in   a,(keyboard)
               rra
               jr   c,not_reset     ; unshifted gives chr

               ld   a,&c9           ; RET opcode
               ld   (main_loop),a   ; return to reset on next instruction
not_reset:
               jr   c,got_key       ; unshifted gives chr

still_esc:     ld   a,&f7
               in   a,(status)
               and  %00100000
               jr   z,still_esc     ; wait until Esc released

               ld   a,&1b           ; Esc character
               jr   got_key
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
               ld   a,&5f           ; cursor block
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
               ld   (ix),a
               jp   (ix)


i_undoc_3:     inc  de              ; 3-byte NOP
i_undoc_2:     inc  de              ; 2-byte NOP
i_undoc_1:     jp   (ix)            ; NOP


read_write_loop:
write_loop:    ld   a,h
               cp   base/256        ; emulator or ROMs above?
               jr   nc,write_trap

zwrite_loop:
main_loop:     ld   a,(de)          ; 7/7/15  - fetch opcode
               inc  de              ; 6/7/11  - PC++
               ld   l,a             ; 4/6/6   - LSB is opcode
               ld   h,msb_table/256 ; 7/7/15  - look-up table
               ld   h,(hl)          ; 7/8/16  - opcode MSB
               jp   (hl)            ; 4/5/9   - execute!
                                    ; = 35T (official) / 40T (off-screen) / 72T (on-screen)

write_trap:    cp   &d0
               jr   nz,write_rom
               ld   a,l
               cp   &12             ; display char?
               jr   z,chr_write
               jp   (ix)
chr_write:     set  7,(hl)          ; display busy
               jp   (ix)

write_rom:     ld   a,&f3           ; Z80 DI opcode
               ld   (base),a        ; emulator appears to be in ROM for RAM tests
               jp   (ix)

read_loop:     ld   a,h
               cp   &d0
               jr   z,read_trap

               ld   a,(de)          ; fetch opcode
               inc  de              ; PC++
               ld   l,a             ; LSB is opcode
               ld   h,msb_table/256 ; look-up table
               ld   h,(hl)          ; opcode MSB
               jp   (hl)            ; execute!

read_trap:     ld   a,l
               cp   &10             ; key read?
               jr   z,key_read
               jp   (ix)
key_read:      inc  l
               res  7,(hl)          ; key not available
               jp   (ix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

load_state:    ld   a,(reg_a)
               ld   b,a             ; set A
               ld   a,(reg_x)
               ld   iyh,a           ; set X to IYh
               ld   a,(reg_y)
               ld   iyl,a           ; set Y to IYl
               exx
               ld   a,(reg_s)
               ld   l,a             ; set S
               ld   h,&01           ; MSB for stack pointer
               ld   a,(reg_p)
               ld   c,a             ; keep safe
               and  %00001100       ; keep D and I
               or   %00110000       ; force T and B
               ld   d,a             ; set P
               ld   a,c
               and  %01000000       ; keep V
               ld   e,a             ; set V
               ld   a,c
               rra                  ; carry from C
               ex   af,af'          ; set carry
               ld   a,c
               and  %10000010       ; keep N Z
               xor  %00000010       ; zero for Z
               exx
               ld   c,a             ; set N Z
               ld   de,(reg_pc)     ; set PC
               ld   ix,main_loop    ; decode loop
               ret

save_state:    ld   a,b             ; get A
               ld   (reg_a),a
               ld   a,iyh           ; get X from IYh
               ld   (reg_x),a
               ld   a,iyl           ; get Y from IYl
               ld   (reg_y),a
               ex   af,af'          ; carry
               inc  c
               dec  c               ; set N Z
               push af              ; save flags
               ex   af,af'          ; protect carry
               exx
               pop  bc
               ld   a,c
               and  %10000001       ; keep Z80 N and C
               bit  6,c             ; check Z80 Z
               jr   z,save_nz
               or   %00000010       ; set Z
save_nz:       or   e               ; merge V
               or   d               ; merge T B D I
               ld   (reg_p),a
               ld   a,l             ; get S
               ld   (reg_s),a
               exx
               ld   (reg_pc),de
               ret


; During running we keep the 65xx registers in Z80 registers
; These are used only to hold the state before/after running
reg_a:         defb 0
reg_p:         defb 0
reg_x:         defb 0
reg_y:         defb 0
reg_s:         defb 0
reg_pc:        defw 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               defs -$\256

msb_table:     defb op_00>>8, op_01>>8, op_02>>8, op_03>>8, op_04>>8, op_05>>8, op_06>>8, op_07>>8
               defb op_08>>8, op_09>>8, op_0a>>8, op_0b>>8, op_0c>>8, op_0d>>8, op_0e>>8, op_0f>>8
               defb op_10>>8, op_11>>8, op_12>>8, op_13>>8, op_14>>8, op_15>>8, op_16>>8, op_17>>8
               defb op_18>>8, op_19>>8, op_1a>>8, op_1b>>8, op_1c>>8, op_1d>>8, op_1e>>8, op_1f>>8
               defb op_20>>8, op_21>>8, op_22>>8, op_23>>8, op_24>>8, op_25>>8, op_26>>8, op_27>>8
               defb op_28>>8, op_29>>8, op_2a>>8, op_2b>>8, op_2c>>8, op_2d>>8, op_2e>>8, op_2f>>8
               defb op_30>>8, op_31>>8, op_32>>8, op_33>>8, op_34>>8, op_35>>8, op_36>>8, op_37>>8
               defb op_38>>8, op_39>>8, op_3a>>8, op_3b>>8, op_3c>>8, op_3d>>8, op_3e>>8, op_3f>>8
               defb op_40>>8, op_41>>8, op_42>>8, op_43>>8, op_44>>8, op_45>>8, op_46>>8, op_47>>8
               defb op_48>>8, op_49>>8, op_4a>>8, op_4b>>8, op_4c>>8, op_4d>>8, op_4e>>8, op_4f>>8
               defb op_50>>8, op_51>>8, op_52>>8, op_53>>8, op_54>>8, op_55>>8, op_56>>8, op_57>>8
               defb op_58>>8, op_59>>8, op_5a>>8, op_5b>>8, op_5c>>8, op_5d>>8, op_5e>>8, op_5f>>8
               defb op_60>>8, op_61>>8, op_62>>8, op_63>>8, op_64>>8, op_65>>8, op_66>>8, op_67>>8
               defb op_68>>8, op_69>>8, op_6a>>8, op_6b>>8, op_6c>>8, op_6d>>8, op_6e>>8, op_6f>>8
               defb op_70>>8, op_71>>8, op_72>>8, op_73>>8, op_74>>8, op_75>>8, op_76>>8, op_77>>8
               defb op_78>>8, op_79>>8, op_7a>>8, op_7b>>8, op_7c>>8, op_7d>>8, op_7e>>8, op_7f>>8
               defb op_80>>8, op_81>>8, op_82>>8, op_83>>8, op_84>>8, op_85>>8, op_86>>8, op_87>>8
               defb op_88>>8, op_89>>8, op_8a>>8, op_8b>>8, op_8c>>8, op_8d>>8, op_8e>>8, op_8f>>8
               defb op_90>>8, op_91>>8, op_92>>8, op_93>>8, op_94>>8, op_95>>8, op_96>>8, op_97>>8
               defb op_98>>8, op_99>>8, op_9a>>8, op_9b>>8, op_9c>>8, op_9d>>8, op_9e>>8, op_9f>>8
               defb op_a0>>8, op_a1>>8, op_a2>>8, op_a3>>8, op_a4>>8, op_a5>>8, op_a6>>8, op_a7>>8
               defb op_a8>>8, op_a9>>8, op_aa>>8, op_ab>>8, op_ac>>8, op_ad>>8, op_ae>>8, op_af>>8
               defb op_b0>>8, op_b1>>8, op_b2>>8, op_b3>>8, op_b4>>8, op_b5>>8, op_b6>>8, op_b7>>8
               defb op_b8>>8, op_b9>>8, op_ba>>8, op_bb>>8, op_bc>>8, op_bd>>8, op_be>>8, op_bf>>8
               defb op_c0>>8, op_c1>>8, op_c2>>8, op_c3>>8, op_c4>>8, op_c5>>8, op_c6>>8, op_c7>>8
               defb op_c8>>8, op_c9>>8, op_ca>>8, op_cb>>8, op_cc>>8, op_cd>>8, op_ce>>8, op_cf>>8
               defb op_d0>>8, op_d1>>8, op_d2>>8, op_d3>>8, op_d4>>8, op_d5>>8, op_d6>>8, op_d7>>8
               defb op_d8>>8, op_d9>>8, op_da>>8, op_db>>8, op_dc>>8, op_dd>>8, op_de>>8, op_df>>8
               defb op_e0>>8, op_e1>>8, op_e2>>8, op_e3>>8, op_e4>>8, op_e5>>8, op_e6>>8, op_e7>>8
               defb op_e8>>8, op_e9>>8, op_ea>>8, op_eb>>8, op_ec>>8, op_ed>>8, op_ee>>8, op_ef>>8
               defb op_f0>>8, op_f1>>8, op_f2>>8, op_f3>>8, op_f4>>8, op_f5>>8, op_f6>>8, op_f7>>8
               defb op_f8>>8, op_f9>>8, op_fa>>8, op_fb>>8, op_fc>>8, op_fd>>8, op_fe>>8, op_ff>>8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               defs -$\256          ; align to 256-byte boundary

; !"#$%&1()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]`abcdefghijklmnopqrstuvwxyz{|}~
font_data:
 MDAT "font.bin"

mask_data:     defb %00000011,%11111111
               defb %11000000,%11111111
               defb %11110000,%00111111
               defb %11111100,%00001111

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end:           equ  $
length:        equ  end-start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ; Instruction implementations
 INC "opimpl.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ; Woz Monitor ROM (&ff00-&ffff)
 ; May be overwritten by includes below
 dump &ff00
 MDAT "apple1.rom"


; Use RAM-based Applesoft BASIC by default
IF 1
    ; Applesoft BASIC [Lite] (6000-7FFF)
    ; http://cowgod.org/replica1/applesoft/
    dump low_page,&6000
    MDAT "applesoft-lite-0.4-ram.bin"
ELSE
    ; Lee Davidson's Enhanced BASIC (5800-77CE)
    ; http://members.lycos.co.uk/leeedavison/6502/ehbasic/
    dump low_page,&5800
    MDAT "ehbasic.bin"
ENDIF


; Use Ken Wessen's BASIC+assembler by default
IF 1
    ; Ken Wessen's custom BASIC + Krusader assembler + enhanced monitor (E000-FFFF)
    ; BRK handler points to mini-monitor in this version
    ; http://school.anhb.uwa.edu.au/personalpages/kwessen/apple1/Krusader.htm
    dump &e000
    MDAT "65C02.rom.bin"
ELSE
    ; Applesoft BASIC [Lite] + Woz monitor (E000-FFFF)
    ; http://cowgod.org/replica1/applesoft/
    dump &e000
    MDAT "applesoft-lite-0.4.bin"
ENDIF


 ; Test program to output the character set (5000-500B)
 ; LDX $00 ; loop: TXA ; JSR echo ; INX ; BRA loop
 dump low_page,&5000
 defb &a2, &00, &8a, &20, &ef, &ff, &e8, &80, &f9
