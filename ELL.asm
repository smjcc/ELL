;;; 
;;; ELL: Embedded Linux Loader
;;;
;;; (c) 2022 Scott Jennings - All rights reserved as per GPLv3+:
;;;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;; 
;;;  Some code adapted from Dr Gareth Owen (www.ghowen.me) and Sebastian Plotz.
;;;  Rewritten, for embedded applications.

;;; Uses ah=0x02 for int13 disk reads (my olden boxen won't do 0x42)
;;;
;;; Expects a valid GUID Partition Table (GPT), with two valid linux
;;; kernels in the first two partitions (no filesystems)
;;; All of both partitions must be within the fisrt 33.5MB of the disk,
;;; as we only use a 16bit logical block address (LBA).
;;; 
;;; A byte at offset 447(0x1bf) in LBA0 (MBR) contains two flag bits
;;; declaring which kernel will boot by default, and whether to boot
;;; the other kernel once, clearing that flag in the MBR (LBA0).
;;;
;;; The kernels in partitions 1 and 2 are presumed to mount the filesystems
;;; in partitions 3 and 4 respectively, and a command line to that effect
;;; is generated.  No support is provided for initrd.
;;;
;;; 
;;; The project here is to boot a kernel in partition 1, mounting
;;; partition 3 as root cramfs, or kernel in partition 2, mounting
;;; partition 4 as root cramfs.
;;; 
;;; This gives us two complete system copies.
;;;
;;; This allows us to upgrade the kernel and root filesystem,
;;; (via busybox "dd" of the kernel and cramfs into two partitions)
;;; while keeping the older stable system should the new one fail.
;;;
;;; Resources permitting, we then mount partition 5 as SWAP, and
;;; partion 6 as overlayfs.
;;;
;;; With a watchdog started by the kernel, and tickled by the OS
;;; only when it believes it is stable (and remotely accessible),
;;; the result should be safely upgradable remotely via dropbear.
;;;
;;; This code assembles using NASM.

;;; if "CMDLINE" is defined, it will consume MBR space, so keep it short.
;;; the last byte (presently '3') must increment to become the next
;;; partition name. So 'root=/dev/sda3' would also work
;;;	%define	CMDLINE	'root=0803',0

;;; if "CMDLINE1CD" is defined, 48 bytes at LBA0+0x1cd thru LBA0+0x1fd,
;;; are assumed to contain the kernel command line, with LBA0+0x1fd=NULL
;;; 
;;; The command line *MUST* start at LBA0:0x1cd, and *MUST* stop at a
;;; *single* NULL exactly at LBA0:0x1fd.
;;;
;;; The "root=" command *MUST* be last, and the last charcter before the
;;; terminating NULL will be "inc"remented to change the root when the
;;; partition 2 kernel is loaded. Pad with *leading* spaces as needed.
;;;
;;; You must write this data into LBA0 using dd or a binary editor or such.
;;; 
;;; This saves code space in the MBR, and allows for a longer command line.
	%define	CMDLINE1CD

	;; progress reporting:
	%define OURNAME1	'E'	; the boot loader has started
	%define OURNAME2	'L'	; A20 and UNREAL achieved
	%define OURNAME3	'L'	; GPT loaded
	%define PART1		'1'	; booting kernel in partition 1
	%define PART2		'2'	; booting kernel in partition 2
	%define OURNAME4	':'	; kernel realmode code loaded
	;; then the kernel version is reported (if enabled)
	;; followed by one '.' for each group of logical blocks loaded
	%define	KLAUNCH		'*' ;report kernel real mode code being launched

	%define	A20ERR		'A' ;failed to enable A20
	%define LBA0WRITEERR	'W' ;failed to write boot sector
	%define	HDDERR		'R' ;failed to read from disk
	%define	KSIGERR		'S' ;failed to verify kernel signature 'HdrS'
	%define	KPROTOERR	'P' ;kernel protocol version incompatible
	%define	KLOADHIERR	'H' ;kernel not to be loaded hi
	%define GPTSIGERR	'G' ;GPT signature not found
	%define	INT15FAILED	'I' ;A20BYINT15 BIOS returned with carry set
	
;;; These define what code is compiled, there isn't room for all of them
;;; in our 448 bytes of code space, so choose wisely ;-)
;;; the 'R' means recommended, 'D' means diagnostic only
;;; 	'?' means hardware specific '(n)' is the bytes added (last I checked)

	%define REPORTSIZES    		; R(0)  report sizes of code blocks
	%define	CHECKGPTSIG		; R(15) verify the GPT signature
	%define	CHECKKERNELSIG		; R(15) check the kernel signature
	%define	ERRORCODES		; R(16) show single letter error codes
	%define	PROGRESS		; R(30) shows loader progress on console
;;;	%define KERNELVERSIONSTRING	; R(21) shows kernel version string
	%define	ONETIMEBOOT		; R(53) support ELL_flags one-time boot
;;;	%define	A20CHECK		; R(19) verify A20 is enabled
;;;	%define	A20CHECKDELAY		; R delay before verifying A20 enabled
;;;	%define	A20BY0XEE		; ? smallest A20 enabler
;;;	%define	A20BYINT15		; ? BIOS A20 enabler
;;;	%define A20BYFASTGATESMALL	; ?
;;;	%define A20BYFASTGATESAFE	; ? bigger than above, but also checks
;;;	%define	A20CHECKEACH		; R check A20 after each set attempt
;;;	%define	LOADWHOLEPARTITION	; D(-10) load the whole kernel partition
;;;	%define	DUMP			; D marginally useful subroutine
;;;	%define	HEXPRINT		; D(32) adds print_dx_hex subroutine
;;;	%define	CHECKBIOSEXT		; D() check BIOS int13 ah=4x support
;;;	%define	CHECKBIOSCHSLIMITS	; D() hack brute force limits discovery
;;;	%define BLOCKS64		; in case 128 doesn't work for you.
;;;	%define PRINTINT	0x21	; D(buggy) use int instr for print_al
;;;
;;; LOADWHOLEPARTITION loads the entire kernel partition, instead
;;; of honoring the kernel paragraph size info, this code is smaller.

;;; ============================================================
;;; don't mess with these, they define code block dependencies
;;; ============================================================

	%undef	PRINTAL		;(8)
	%undef	PRINTSTRING	;(18)
	%undef	A20SET

	%ifdef	CMDLINE
	 %ifdef	CMDLINE1CD
	  %error only one of "CMDLINE" and "CMDLINE1CD" can be defined
	 %endif	;CMDLINE1CD
	%endif	;CMDLINE

	%ifdef	A20CHECKDELAY
	 %ifndef A20CHECK
	  %error "A20CHECKDELAY" has no effect without "A20CHECK"
	 %endif	;A20CHECK
	%endif	;A20CHECKDELAY

	%ifdef	A20BY0XEE
	 %define A20SET
	%endif	;A20BY0XEE
	%ifdef	A20BYINT15
	 %define A20SET
	%endif	;A20BYINT15
	%ifdef	A20BYFASTGATESMALL
	 %define A20SET
	%endif	;A20BYFASTGATESMALL
	%ifdef	A20BYFASTGATESAFE
	 %define A20SET
	%endif	;A20BYFASTGATESAFE
	%ifndef	A20SET
	 %ifdef	A20CHECKEACH
	  %error "A20CHECKEACH" has no effect without code to set A20 enabled
	 %endif	;A20CHECKEACH
	%endif	;A20SET
	
	%ifdef	ERRORCODES
	 %define  PRINTAL
	%endif  ;ERRORCODES

	%ifdef	PROGRESS
	%define  PRINTAL
	%endif ;PROGRESS

	%ifdef	KERNELVERSIONSTRING
	%define	 PRINTSTRING
	%endif ;KERNELVERSIONSTRING

	%ifdef	CHECKBIOSEXT
	 %define DIAGNOSTICONLY
	 %define PRINTSTRING
	 %define HEXPRINT
	%endif	;CHECKBIOSEXT

	%ifdef	CHECKBIOSCHSLIMITS
	 %define DIAGNOSTICONLY
	 %define HEXPRINT
	 %define PRINTAL
	%endif	;CHECKBIOSCHSLIMITS

	%ifdef	DIAGNOSTICONLY
	 %undef	ERRORCODES
	 %undef	PROGRESS
	 %undef	CHECKGPTSIG
	 %undef	CHECKKERNELSIG
	 %undef	A20CHECK
 	 %undef	ONETIMEBOOT
	 %undef	DUMP
	%endif	;DIAGNOSTICONLY

	%ifdef	HEXPRINT
	 %define  PRINTAL
	%endif  ;HEXPRINT

	%ifdef	PRINTSTRING
	 %define PRINTAL
	%endif	;PRINTSTRING
	
;;; ============================================================
	
;;; This is the beginning of the MBR code
;;; This is offset 0 in LBA0 of the boot disk.
;;; The BIOS loads it to 0x0000:0x7c00

	[BITS 16]

org	0x7c00
;;; 	cli			; dog do we need this?
	mov	sp, 0x7c00	; setup stack
	xor	ax, ax
	mov	ss, ax
	mov	ds, ax

%ifdef	PRINTINT	 ;kernel is unhappy with this....
	;; set int PRINTINT to point to call print_al
	;; because int is one byte smaller than call
	mov	word [PRINTINT*4+2], ax
	mov	word [PRINTINT*4], print_al
%endif	;PRINTINT

;;;	cld			; dog do we need this?
;;; save the boot device number passed from the BIOS
	mov	byte [boot_drive], dl

;;; Read Drive Parameters (ah=0x08) to get the sectors per head
	mov	ah, 0x08
	int	0x13
	and	cl, 0x3f
	mov	[secs_per_head], cl

%ifdef	PROGRESS
	mov	al, OURNAME1	; the boot loader has started
	%ifdef	PRINTINT
	int	PRINTINT	; print_al
	%else
	call	print_al
	%endif	;PRINTINT
%endif	;PROGRESS

;;; ====================================================
;;; FOLLOWING %ifdef BLOCKS ARE OPTIONAL DIAGNOSTIC CODE
;;;    THEY ALL REPORT DATA TO CONSOLE AND THEN HANG
;;; ====================================================
	%ifdef	CHECKBIOSEXT
;;; -----------------------------------------------------------
;;; This diagnostic code checks for BIOS support of the
;;; int 0x13 EXTensions. If these exist, a more direct
;;; method of LBA disk reading is available.
;;;
;;; If they do not exist, ah=0x2 must be used instead
;;; and LBA must be converted to C/H/S, using data from
;;; another BIOS call to read the Drive Parameters
;;;
;;; This code simply prints the data returned from the BIOS
;;; and then hangs. Useful on embedded systems with a console.
;;; -----------------------------------------------------------
	mov	word bx, 0x55aa
	mov	dl, [boot_drive]
	mov	ah, 0x41 	;see if extentions available
	int	0x13
	push	cx
	push 	bx
	push	ax

	mov	si, .EXTFAIL
	jnc	short .nofail

	call	print_string

.nofail:
	mov	si, .EXT
	call	print_string
	pop	dx
	call	print_dx_hex
	pop	dx
	call	print_dx_hex
	pop	dx
	call	print_dx_hex
	jmp	$
;;; 	---------
.EXTFAIL	db	'EXTfail',0xd,0xa,0
.EXT		db	'ax, bx, cx:',0xd,0xa,0
	%endif	;CHECKBIOSEXT

	%ifdef	CHECKBIOSCHSLIMITS
;;; ------------------------------------------------------
;;; This ugly hack was an attempt to discovery the maximum
;;; number of sectors and heads by brute force.
;;; ------------------------------------------------------
	xor	ax, ax
	mov	bx, ax 		;offset0
	mov	cx, ax		;cyhi0+sec
	mov	dx, ax		;head0
	push	0x1000
	pop	es
	inc	al		;read one sector
	
	.secloop:
	inc	cl
	mov	ah, 0x02	;read drive sectors command
	mov	dl, [boot_drive]
	pusha
	int	0x13
	popa
 	jc	.secfail

	or	cl, cl
	jnz	.secloop

	.secfail:
	pusha
	mov	dx, cx
	call	print_dx_hex
	popa
	mov	cx, 1		;sector 1
	pusha

	.hdloop:
	pusha
	int	0x13
	popa
	jc	.hdfail

	inc	dh
	jnz	.hdloop

	.hdfail:
	mov	dl, dh
	xor	dh, dh
	call	print_dx_hex

	mov	al, '!'
	call	print_al
	jmp	$
;;; 	---------
%endif	;CHECKBIOSCHSLIMITS

;;; =============================================
;;;   END OF OPTIONAL DIAGNOSTIC BLOCKS OF CODE
;;; WE NOW RETURN YOU TO YOUR REGULAR BOOT LOADER
;;; =============================================

;;; -------------------------------------------------------------------
;;; The kernel is large, and must be loaded high so we must enable the
;;; high address bit: A20, and enter protected mode to get access to
;;; the upper memory address space.
;;; 
;;; Some systems start with A20 already enabled, so we can check that
;;; and avoid enabling code which could be problematic.
;;; This code takes up precious MBR space though.
;;; 
;;; There are multiple methods of enabling A20 and each have their
;;; issues. Some recommend trying all of them in order of increasing
;;; risk, and checking for success as you go. There can be delays on
;;; some systems before A20 is actually enabled. That's a lot of code.
;;; We don't really have that kind of space in the MBR, so a compromise
;;; must be struck, and that should be *your* choice.
;;; -------------------------------------------------------------------

;;; NOTE: CALLS TO check_a20 CRASH ES!
	
%ifdef	A20SET	;if needed, we will use at least one method to enable A20
 %ifdef	A20CHECK  ; we will check first if it's already enabled
	call	check_a20
	jne	short .a20_is_enabled
 %endif	;A20CHECK

 %ifdef	A20BY0XEE
;;; the "0xee" method (read/write strobe to 0xee) (al content irrelevent)
;;;	out	0xef, al		;RESET (for information only)
	in	al, 0xee		;Enable A20
;;;	out	0xee, al		;Disable A20 (for information only)
  %ifdef A20CHECKEACH
   %ifdef A20CHECKDELAY
	call	delay_check_a20
	jne	short .a20_is_enabled
   %else
	call	check_a20
	jne	short .a20_is_enabled
   %endif ;A20CHECKDELAY
  %endif ;A20CHECKEACH	
 %endif	;A20BY0XEE

 %ifdef A20BYFASTGATESMALL
;;; the "fast A20 gate" method (io data at 0x92)
;;; quick and dirty version
;;; note: bit 0 of 0x92 is RESET
 	in	al, 0x92
 	or	al, 2
	out	0x92, al	;Enable A20
  %ifdef A20CHECKEACH
   %ifdef A20CHECKDELAY
	call	delay_check_a20
	jne	short .a20_is_enabled
   %else
	call	check_a20
	jne	short .a20_is_enabled
   %endif ;A20CHECKDELAY
  %endif ;A20CHECKEACH	
 %endif  ;A20FASTGATESMALL
 %ifdef	A20BYFASTGATESAFE
;;; safer use of fast A20 gate
	in	al, 0x92
	test	al, 2		;A20 already enabled?
	jnz	short .a20_is_enabled

	or	al, 2		;set A20 enable
	and	al, 0xFE	;unset RESET
	out	0x92, al	;enable A20
  %ifdef A20CHECKEACH
   %ifdef A20CHECKDELAY
	call	delay_check_a20
	jne	short .a20_is_enabled
   %else
	call	check_a20
	jne	short .a20_is_enabled
   %endif ;A20CHECKDELAY
  %endif ;A20CHECKEACH	
 %endif	;A20BYFASTGATESAFE

 %ifdef	A20BYINT15
;;; BIOS int15 method
	mov	ax,0x2403	;first check if supported.
	int	0x15		;maybe this can be left out?
  %ifdef ERRORCODES
	mov	al, INT15FAILED
  %endif ;ERRORCODES
	jb	err_al	;int15 unsupported?

	cmp	ah, 0
	jnz	err_al

	mov	ax, 0x2401 ; A20 line enable via BIOS
	int	0x15
  %ifdef ERRORCODES
	mov	al, INT15FAILED
  %endif ;ERRORCODES
	jc	err_al	;int15 a20 enable failed
  %ifdef A20CHECKEACH
   %ifdef A20CHECKDELAY
	call	delay_check_a20
	jne	short .a20_is_enabled
   %else
	call	check_a20
	jne	short .a20_is_enabled
   %endif ;A20CHECKDELAY
  %endif ;A20CHECKEACH
 %endif ;A20BYINT15

 %ifdef A20CHECK		;did A20 finally get enabled?
  %ifndef A20CHECKEACH
   %ifdef A20CHECKDELAY
	call	delay_check_a20
	je	err_al
   %else
	call	check_a20
	je	err_al
   %endif ;A20CHECKDELAY
  %endif ;A20CHECKEACH
 %endif ;A20CHECK
%else ;A20SET undefined
 %ifdef A20CHECK
	call	check_a20
	je	err_al
 %endif ;A20CHECK
%endif	;A20SET else

.a20_is_enabled:
	lgdt	[gdt_desc]	;load the global/interupt descriptor table
	mov	eax, cr0
	or	al, 00000001b	; set protected mode bit
	mov	cr0, eax	; enter protected mode
	jmp	short $+2

	mov	bx, 0x8 	; first descriptor in GDT
	mov	ds, bx
	mov	es, bx
	mov	gs, bx

	and	al, 11111110b	; clear protected mode bit
	mov	cr0, eax	; back to real mode

;;; 	- now limits are removed but seg regs still work as normal

	xor	ax, ax 		; restore segment values
 	mov	ds, ax
	mov	gs, ax
	mov	es, ax		; segment to load MBR and GPT into
;;; 	sti			; finally dog do I need this?

;;; now in UNREAL mode
	%ifdef	PROGRESS
	mov	al, OURNAME2	; A20 and UNREAL achieved
	 %ifdef	PRINTINT
	int	PRINTINT	; print_al
	 %else
	call	print_al
	 %endif	;PRINTINT
	%endif	;PROGRESS

;;; load the MBR, and GPT (LBA0, LBA1, and LBA2)
	xor	ax, ax		; starting with LBA=0
	mov	dl, 3		; three sectors (LBA0-LBA2)
	mov	bx, MBR		; offset (in es which is 0x0000)
	call	disk_read	; load first three sectors into 0:MBR
	%ifdef	PROGRESS
	mov	al, OURNAME3	; GPT loaded
	 %ifdef	PRINTINT
	int	PRINTINT	; print_al
	 %else
	call	print_al
	 %endif	;PRINTINT
	%endif	;PROGRESS

%ifdef	CHECKGPTSIG
	.CHECKGPTSIG:
%ifdef	ERRORCODES
	mov	al, GPTSIGERR
%endif	;ERRORCODES
;;; 	cmp	dword [GPT_sig], 'EFI '
;;;	cmp	word [GPT_sig+2], 'I ' ;dog save three bytes
	cmp	byte [GPT_sig], 'E' ;dog save four bytes
	jne	err_al
	%ifdef	REPORTSIZES
	%assign	size $-.CHECKGPTSIG
	%warning "CHECKGPTSIG" added size bytes
	%endif	;REPORTSIZES
%endif	;CHECKGPTSIG

%ifdef	ONETIMEBOOT
	.ONETIMEBOOT:
	mov	al, byte [MBR_ELL_flags] ;the disk copy of ELL_flags
	test	al, 00000010b 		; is this a one time boot?
	jz	short .default_partition

;;; if this is a one time boot, then clear the one time boot bit on disk
;;; and switch to the "other" (not default) kernel partition

	and	byte [MBR_ELL_flags], 11111101b ;clear the one time bit
;;;	and	al, 11111101b ;clear the bit	;same size code... meh...
;;;	mov	byte [MBR_ELL_flags], al

	push	ax
	mov	bx, MBR		; offset
	xor	cx, cx		;0 sec/trk
	mov	dx, cx		;0 hd
	inc	cl		;first sector is 1
	mov	dl, [boot_drive]
	mov	ax, 0x0301	;write one sector
	int	0x13		;write the modified MBR sector back
%ifdef	ERRORCODES
	mov	al, LBA0WRITEERR
%endif	;ERRORCODES
	jc	err_al	; doggy should this be an error? we could still boot..

	pop	ax
;;;	mov	al, byte [MBR_ELL_flags] ; push/pop is one byte smaller.
	xor	al, 00000001b	;toggle the default kernel partition

.default_partition:
	test	al, 00000001b	;which partition to load kernel from
	%ifdef PROGRESS
	mov	al, PART1
	%endif ;PROGRESS
	jz	.part1

	mov	ax, [part2_FLBA]
	mov	[part1_FLBA], ax
	%ifdef	LOADWHOLEPARTITION
	mov	ax, [part2_LLBA]
 	mov	[part1_LLBA], ax
	%endif	;LOADWHOLEPARTITION
	%ifdef	CMDLINE
	inc	byte [cmdLine+cmdLineLen-2] ;use the other root filesystem
	%endif
	%ifdef	CMDLINE1CD
	inc	byte [command_line+48-1]
	%endif
	%ifdef PROGRESS
	mov	al, PART2
	%endif ;PROGRESS

.part1:
	%ifdef PROGRESS
	 %ifdef	PRINTINT
	int	PRINTINT	; print_al
	 %else
	call	print_al
	 %endif	;PRINTINT
	%endif ;PROGRESS
	%ifdef	REPORTSIZES
	%assign	size $-.ONETIMEBOOT
	%warning "ONETIMEBOOT" added size bytes
	%endif	;REPORTSIZES
%endif	;ONETIMEBOOT

;;; load the kernel header and real mode code to 0x1000

	push	0x1000
	pop	es		; es=0x1000 ds=0x0000
	mov	ax, [part1_FLBA] ; first LBA of partition one
	mov	dl, 64		;maximum kernel header size of 32K (64 sectors)
	call	disk_read0	;load the first 32K block of the kernel

%ifdef	PROGRESS
	mov	al, OURNAME4	; kernel realmode code loaded
	%ifdef	PRINTINT
	int	PRINTINT	; print_al
	%else
	call	print_al
	%endif	;PRINTINT
%endif	;PROGRESS

	push	ds		;0x0000
	push	es		;0x1000
	pop	ds		; ds = es = 0x1000, 0x0000 on stack

%ifdef	CHECKKERNELSIG
	.CHECKKERNELSIG:
;;;	cmp	dword [0x202], 'HdrS' ;signature
;;;	cmp	word [0x202+2], 'rS' ;signature dog save three bytes
	cmp	byte [0x202], 'H' ;signature dog save four bytes
%ifdef	ERRORCODES
	mov	al, KSIGERR
%endif	;ERRORCODES
	jne	err_al
	%ifdef	REPORTSIZES
	%assign	size $-.CHECKKERNELSIG
	%warning "CHECKKERNELSIG" added size bytes
	%endif	;REPORTSIZES
%endif	;CHECKKERNELSIG

	cmp	word [0x206], 0x204	; kernel boot protocol version
%ifdef	ERRORCODES
	mov	al, KPROTOERR
%endif	;ERRORCODES
	jb	err_al			; must be protocol v2.04 or greater

%ifdef	ERRORCODES
	mov	al, KLOADHIERR
%endif	;ERRORCODES
	test	byte [0x211], 1 	; loadflags: boot protocol option flags
 	jz	err_al			; error if not loaded high: 0x100000

%ifdef	KERNELVERSIONSTRING
	.KERNELVERSIONSTRING:
	mov	si, [0x20e]	; pointer to kernel version string
	add	si, 0x200	; minus kernel boot sector
	call	print_string	;print the kernel version string
	%ifdef	REPORTSIZES
	%assign	size $-.KERNELVERSIONSTRING
	%warning "KERNELVERSIONSTRING" added size bytes
	%endif	;REPORTSIZES
%endif	;KERNELVERSIONSTRING
;;;
;;; 	obligatory kernel setup ( ds = es = 0x1000, 0x0000 on stack)
;;;
%ifdef	ERRORCODES
	mov	word [0x210], 0x81e1	; set LOADHI+USEHEAP+type_of_loader
%else
	;; set kernel quiet mode if ERRORCODES not enabled
	mov	word [0x210], 0xa1e1	; set LOADHI+USEHEAP+type_of_loader
%endif	;ERRORCODES

;;; 	if you want play with initrd:
;;;	mov	dword [0x218], 0x0000 ;set ramdisk_image
;;;	mov	dword [0x21C], 0x0000 ;set ramdisk_size

	mov	word [0x224], 0xde00	 ;set heap_end_ptr
;;;	mov	byte [0x226], 0x00	 ;set ext_loader_ver
	mov	byte [0x227], 0x01	 ;set ext_loader_type / bootloader id 11
 	mov	dword [0x228], 0x1e000 ; set cmd_line_ptr

	pop	ds		       ; ds=0x0000 es=0x1000

;;; copy cmd line
	%ifdef	CMDLINE
	mov	si, cmdLine
	mov	cx, cmdLineLen
	%endif	;CMDLINE
	%ifdef	CMDLINE1CD
	mov	si, command_line
	mov	cx, 48+1	;command_line plus NULL terminator
	%endif	;CMDLINE1CD
	mov	di, 0xe000
	rep	movsb ; copies from DS:si to ES:di (0x1e000)

    ; the protected mode part must be loaded at 0x100000
    ; load 128 sectors at a time to 0x2000, then copy to 0x100000

;;; load the kernel protected mode code to 0x100000

%ifndef	LOADWHOLEPARTITION
 	mov	edx, [es:0x1f4] ; 16byte pages of protected code to load
;;; /32 (pages per block) to get blocks, then by 128 to get 128 block groups
	%ifdef	BLOCKS64
  	shr	edx, 11		; /2048 = number of 64block groups to load
	%else
 	shr	edx, 12		; /4096 = number of 128block groups to load
	%endif	;BLOCKS64
	inc	edx		; +1 potentially partial group

	mov	[load_counter], dx ;save the number of 128block groups to load
%endif	;not LOADWHOLEPARTITION
 	mov 	al, [es:0x1f1] ; no of blocks in the kernel realmode header
 	or	al, al
 	jnz	short .notdefaultsize

	mov	al, 4		;default size

.notdefaultsize:
	xor	ah,ah ; can I assume ah=0? set last by bios read status maybe?
	add	ax, [part1_FLBA] ;compute LBA of start of protected mode code
	inc	ax		 ;plus one for the legacy boot sector
	push	0x2000		 ;segment to load block groups into
	pop	es		 ;es=0x2000 ds=0x1000

.loadloop:
	%ifdef	BLOCKS64
	mov	dl, 64 ;blocks per group
	%else
	mov	dl, 128 ;blocks per group
	%endif	;BLOCKS64
	push	ax     ;save the current LBA
	call	disk_read0

;;; highmove:
	mov	esi, 0x20000
	mov	edi, [highmove_addr]
	%ifdef	BLOCKS64
	mov	edx, 512*64/4
	%else
	mov	edx, 512*128/4
	%endif	;BLOCKS64
.highloop:
	mov	eax, [ds:esi]
	mov	[ds:edi], eax
	add	esi, 4
	add	edi, 4
	dec	edx
	jnz	short .highloop

	mov	[highmove_addr], edi
%ifdef	PROGRESS
	mov	al, '.'
	%ifdef	PRINTINT
	pusha
	int	PRINTINT	; print_al
	popa
	%else
	call	print_al
	%endif	;PRINTINT
%endif	;PROGRESS
	pop	ax
	%ifdef	BLOCKS64
	add	ax, 64		; next LBA to load from
	%else
	add	ax, 128		; next LBA to load from
	%endif	;BLOCKS64
%ifdef	LOADWHOLEPARTITION
	cmp	ax, [part1_LLBA]
 	jng	short .loadloop
%else
 	dec	word [load_counter]
	jnz	short .loadloop
%endif	;LOADWHOLEPARTITION

%ifdef	PROGRESS
	mov	al, KLAUNCH	; starting the Kernel
	%ifdef	PRINTINT
	int	PRINTINT	; print_al
	%else
	call	print_al
	%endif	;PRINTINT
%endif	;PROGRESS
kernel_start:
	cli
	mov	ax, 0x1000
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax
	mov	sp, 0xe000
	jmp	0x1020:0	;and away we go!
;;; 	----------------

;;;------------------------------------------------
;;; error handling
;;;------------------------------------------------

err_al:
	%ifdef ERRORCODES
	 %ifdef	PRINTINT
	int	PRINTINT	; print_al
	 %else
	call	print_al
	 %endif	;PRINTINT
	%else
	 %ifdef PROGRESS
	mov	al, '!'		;if PROGRESS, then print_al, so show anon err
	  %ifdef PRINTINT
	int	PRINTINT	; print_al
	  %else
	call	print_al
	  %endif ;PRINTINT
	 %endif	;PROGRESS
	%endif 	;ERRORCODES
err:	jmp	short $		;hang forever
	;; ----

%ifdef PRINTSTRING
;;;---------------------------------------------------------
;;; on entry ds:si points to NULL terminated string to print
;;;---------------------------------------------------------
print_string:
	lodsb
	or	al, al
	jz	short return

	%ifdef	PRINTINT
	int	PRINTINT	; print_al
	%else
	call	print_al
	%endif	;PRINTINT
	jmp	short print_string
;;; 	-------------
	%ifdef	REPORTSIZES
	%assign	size $-print_string
	%warning "PRINTSTRING" added size bytes
	%endif	;REPORTSIZES
%endif ;PRINTSTRING
;;;------------------------------------------------

%ifdef DUMP
dump:
	mov 	ax, 0x1000
	mov	ds, ax
	xor	si, si
	mov	cx, 512

.loop:
	lodsb
	mov	ah, 0xe
	mov	bx, 7
	push	cx
	int	0x10
	pop	cx
	dec	cx
	jnz	short .loop

	jmp	short $
;;;	---------
	%ifdef	REPORTSIZES
	%assign	size $-dump
	%warning "DUMP" added size bytes
	%endif	;REPORTSIZES
%endif	;DUMP

;;;------------------------------------------------

%ifdef HEXPRINT			;MUST PRECEDE PRINTAL
pdxh:
	call	.dh
	
	mov	dh, dl
.dh:
	mov	al, dh
	shr	al, 4
	call	.all

	mov	al, dh
.all:
	and	al, 0x0f
	or	al, 0x30
	cmp	al, '9'
	jng	short print_al

	add	al, 'a' - '9' - 1
	jmp	short print_al
;;; 	----------------

print_dx_hex:
	call	pdxh
	mov	al, ' '
	%ifdef	REPORTSIZES
	%assign	size $-pdxh
	%warning "HEXPRINT" added size bytes
	%endif	;REPORTSIZES
%endif	;HEXPRINT		;MUST PRECEDE PRINTAL

%ifdef	PRINTAL			;MUST FOLLOW HEXPRINT
print_al:
	mov	ah, 0xe
	mov	bx, 7
	int	0x10
return:
	ret
;;;	---
	%ifdef	REPORTSIZES
	%assign	size $-print_al
	%warning "PRINTAL" added size bytes
	%endif	;REPORTSIZES
%endif	;PRINTAL		;MUST FOLLOW PRINTHEX


;;;------------------------------------------------
;;;
;;; READ UP TO 128 LOGICAL BLOCKS USING BIOS INT13 AH=0x02 Cyl/Hds/Sec
;;;
;;; on entry:
;;; ax=FirstLBA dl=CountOfBlocksToRead es:bx=DestinationAddress
;;; !! assumes ds=0x0000 !!
;;; crashes: BIOS, so who knows... should preserve segment regs
;;;
;;; with a 16bit LBA address, we get to the first 33.5MB of HDD
;;; ( (2**16)*512 = 33554432 ) which should be enough for two kernels,
;;; one in each of the first two partitions
;;;
;;; valid ranges: sectors: 1-32  heads: 0-255
;;;		  CountOfBlocksToRead: 1-128 (up to one full 64K segment)
;;;
;;; for example: if LBA=34 then cyl=0, head=1, sec=3
;;;

disk_read0:
	xor	bx, bx		;null offset into destination segment
disk_read:
;;; convert LBA in dx to c/h/s in ch/dh/cl

	div	byte [secs_per_head]
	xor	cx, cx
	mov	cl, ah
 	inc	cl		;sectors start with 1
	mov	dh, al		;head
	mov	al, dl		;number of sectors to read

;;; now ch=2bits of cyl, dh=8bits of head, cl=5bits of sector

	mov	ah, 0x02	;read drive sectors command
	mov	dl, [boot_drive]
	int	0x13
	%ifdef	ERRORCODES
	mov	al, HDDERR
	%endif	;ERRORCODES
	jc	short err_al

	ret
;;;	---

;;;------------------------------------------------
	
%ifdef A20CHECK
;;; upon return, will jne if A20 is enabled, je if A20 is likely disabled
;;; crashes ax, es, and maybe cx
A20CHECK_start:
 %ifdef A20CHECKDELAY
;;; wait to see if it eventually enables
delay_check_a20:	
 	xor	cx, cx	; arbitrary delay loop value
.delay_loop:
	call	check_a20
	jne	short return

	dec	cx
	jnz	short .delay_loop
 %endif ;A20CHECKDELAY
check_a20:
	xor	ax, ax
	not	ax
	mov	es, ax		    ; 0xffff
	cmp	word [es:0x7e0e], 0xaa55 ; boot sector ID mirrored in himem?
 %ifdef ERRORCODES
	mov	al, A20ERR
 %endif ;ERRORCODES
	ret
;;;	---

 %ifdef UNDEFINED		;another method, more sure, too big.
	xor	ax, ax
;;;	mov	ds, ax
	not	ax ; ax = 0xFFFF
	mov	es, ax
 
	mov	si, 0x0500
	mov	di, 0x0510

	mov	byte [ds:si], 0x3A ;write one
	mov	byte [es:di], 0xA3 ;write two

	cmp 	byte [ds:si], 0x3A
	jne	short check_a20.ret	;write two clobber write one?

	cmp	byte [es:di], 0xA3 ;write two fail to write?
.ret:
	ret
 %endif	;UNDEFINED
	%ifdef	REPORTSIZES
	%assign	size $-A20CHECK_start
	%warning "A20CHECK" added size bytes
	%endif	;REPORTSIZES
%endif	;A20CHECK

;;;------------------------------------------------
;;;  start of initialized data
;;;------------------------------------------------

	;; initialized here to safe code space
highmove_addr:	dd	0x100000 ;pointer to protected mode code destination

;;;------------------------------------------------

;descriptor needed to set up protected mode
gdt_desc:
	dw	gdt_end - gdt - 1
	dd	gdt

;;; access byte:
;;; [present, priv[2] (0=highest), 1, Execbit, Direction=0, rw=1, accessed=0]

;;; flags:
;;; Granuality (0=limitinbytes, 1=limitin4kbs), Sz= [0=16bit, 1=32bit], 0, 0
gdt:
	dq	0 ; first entry 0
;flat data segment
	dw	0FFFFh ; limit[0:15] (aka 4gb)
	dw	0      ; base[0:15]
	db	0      ; base[16:23]
	db	10010010b  ; access byte 
	db	11001111b    ; [7..4]= flags [3..0] = limit[16:19]
	db	0 ; base[24:31]
gdt_end:

;;;------------------------------------------------
	%ifdef	CMDLINE
cmdLine:	db	CMDLINE
cmdLineLen:	equ	$-cmdLine
	%endif

;;; ---------------------------------------------------
;;;
;;; Assuming that the object code is copied to the boot disk using
;;; "dd if=disk of=/dev/sd?", and that said disk was previously
;;; partitioned as GPT, then a "protective partition table"
;;; and the 0xaa55 signature are already there. Therefore, we
;;; stop at 448 to avoid overwriting the "PPT"
;;;
;;; without the 0xaa55 we don't really need this, but it may "initialize"
;;; some of the uninitialized data to zero, looks cleaner on disk, and
;;; very helpfully generates assembly errors when we have too much code.

	%assign MBREND	448	;THIS IS OUR COMPLETE CODESPACE
	%assign REALELLFLAGS MBREND-1
	%assign	freecodespace	MBREND-($-$$)-1
	%ifdef	ONETIME
	%warning "ELL_flags" is at REALELLFLAGS, freecodespace bytes available
	%else
	%warning freecodespace bytes available
	%endif	;ONETIME
 	times	MBREND-($-$$)-1	db	0 ;the -1 is for ELL_flags
;;; --------------------------------------------------
;;; The ELL_flags byte is in LBA0 at offset 447 (0x1be)
;;;
;;; bits defined so far are:
;;;
;;;	0 = partiton to load the kernel from by default
;;;	1 = boot the other partition once, clearing this bit
;;; --------------------------------------------------
ELL_flags:	db	01110010b ;seven is just a sort of sig right now
;;;------------------------------------------------
;;;  start of uninitialized data
;;;------------------------------------------------
 	absolute	$

boot_drive:	resb	1	;BIOS provided boot disk device number
secs_per_head:	resb	1	;BIOS provided boot disk parameter

	%ifndef	LOADWHOLEPARTITION
load_counter:	resw	1	;logical block counter loading protected kernel
	%endif	;LOADWHOLEPARTITION	;

;;; We load three sectors, LBA0-LBA2 to this addres in segment 0x0000
;;; this then, is an actual copy of the disk MBR, and GPT

MBR:
	times	0x1bf	resb	1
;;; This is the ELL flag byte at MBREND-1 on disk
MBR_ELL_flags:	resb	1
	times	0x1cd-($-MBR)	resb	1

;;; This is a 48 byte kernel command line string.
;;; It is crashing the end of the Protective Partition Table (PPT)
;;; The kernel, and all GPT partition editors I've tried so far
;;; do not seem to complain about it. "parted" will call it a "hybrid PPT",
;;; but does not complain about the trash data after the first 0xEE partition
;;;
;;; The command line *MUST* start at LBA0:0x1cd, and *MUST* stop at a
;;; *single* NULL exactly at LBA0:0x1fd. The line must be exactly 48 characters.
;;;
;;; The "root=" command *MUST* be last, and the last charcter before the
;;; terminating NULL will be "inc"remented to change the root when the
;;; partition 2 kernel is loaded. Pad with *leading* spaces as needed.

command_line:	resb	48+1	;0x1cd thru 0x1fd push a NULL
	times	0x200-($-MBR)	resb	1 ; bump to LBA1
	
;;; the GPT (GUID Partition Table) header (LBA1)
GPT_header:
GPT_sig:
	times	0x200	resb	1 ; bump to LBA2

;;; the GPT (GUID Partition Table) entries (LBA2)
GPT_entries:
	times	0x20	resb	1
part1_FLBA:	resw	1	;0x20 GPT part1 First LBA
	times	6	resb	1
part1_LLBA:	resw	1	;0x28 GPT part1 Last LBA
	times	0x80+0x20-($-GPT_entries)	resb	1
part2_FLBA:	resw	1	;GPT part2 First LBA
	times	6	resb	1
part2_LLBA:	resw	1	;GPT part2 Last LBA
	times	512-($-GPT_entries)		resb	1
