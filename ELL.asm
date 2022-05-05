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
;;; Expects a valid GUID Partition Table (GPT), with a valid linux
;;; kernel in the first or first two partitions (no filesystems)
;;; All kernel partitions must be within the fisrt 33.5MB of the disk,
;;; as we only use a 16bit logical block address (LBA).
;;; 
;;; A byte at offset MBREND-1 in LBA0 (MBR) contains two flag bits
;;; declaring which kernel will boot by default, and whether to boot
;;; the other kernel once, clearing that flag in the MBR (LBA0).
;;;
;;; The kernels in partitions 1 and 2 are presumed to mount the filesystems
;;; in partitions 3 and 4 respectively, and a command line to that effect
;;; is generated. No loading of an initrd is supported.
;;; I use the kernel built in initramfs when an initrd is needed.
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
;;;
;;; FYI: search for "doggy" to find potentially unresolved issues.
;;; 	 search for "dog" to find potential issues.
;;; 
;;; if "CMDLINE" is defined as a string, it will consume MBR space,
;;; so keep it short. the last byte (presently '3') must increment
;;; to become the next partition name. So 'root=/dev/sda3' would
;;; also work.
;;; 
;;; if "CMDLINEPPT" is defined, the kernel command line is stored at
;;; the defined byte offset into LBA0 (the MBR), up to the MBR signature
;;; word at LBA0+0x1fe (with the terminating NULL at LBA0+0x1fd)
;;; 
;;; If ONETIMEBOOT is defined, the "root=" command *MUST* be last,
;;; and the last charcter before the terminating NULL will be bytewise
;;; "inc"remented to change the root partition when the kernel in
;;; partition 2 is booted.
;;;
;;; All of the command line will be used, so pad with *leading* spaces.
;;;
;;; You must write this data into LBA0 on the boot media, using dd or a
;;; binary editor or such.
;;; 
;;; This saves code space in the MBR, and allows for a longer command line,
;;; by partially crashing the end of the Protective Partition Table (PPT).
;;; The kernel, and every partition editor tried so far has had no
;;; complaints. "fdisk" (from util-linux) will call it a "hybrid PPT",
;;; but does not complain about the trash data after the first 0xEE
;;; partition. Your mileage may vary.

;;; Memory Usage:
;;; 0x?????? - 0x07bff	stack
;;; 0x007c00 - 0x007dff	ELL (446 bytes,Protective Partition Table, 0x55aa)
;;; 0x010000 - 0x017fff	Real mode kernel		; size: 32Kb
;;; 0x018000 - 0x01dfff	kernel heap			; size: 24Kb
;;; 0x01e000 - 0x01ffff	was command line, now unused	; size: 8Kb
;;; 0x020000 - 0x02ffff	disk read buffer for high moves ; size: 64Kb
;;; 0x100000 - +kernlen	protected-mode kernel
	
;;; only one of these may be defined
;;; 	%define	CMDLINE	'root=0803'
	%define	CMDLINEPPT	462 	; crash last 3 partitions in the PPT

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
	;; errors:
	%define	A20ERR		'A' ;failed to enable A20
	%define LBA0WRITEERR	'W' ;failed to write boot sector (ONETIMEBOOT)
	%define	HDDERR		'R' ;failed to read from disk
	%define GPTSIGERR	'G' ;GPT signature not found
	%define	KSIGERR		'K' ;kernel signature not found
	%define	KPROTOERR	'P' ;kernel protocol version incompatible
	%define	KLOADHIERR	'H' ;kernel not to be loaded hi
	
;;; These define what code is compiled. There isn't room for all of them
;;; in our 446 bytes of code space, so choose wisely ;-)
;;; the 'R' means recommended, 'D' means diagnostic only
;;; 	'?' means hardware specific '(n)' is the bytes added (last I checked)
;;; 	(because of interplay between these, the byte count is an estimate)

	%define REPORTSIZES    		; R(0)  report sizes of code blocks
	%define	CHECKGPTSIG		; R(15) verify the GPT signature
	%define	CHECKKERNELSIG		; R(15) check the kernel signature
	%define	SIGSIZE	2		; R Bytes of signatures to check 1,2,or4
;;;	%define CHECKPROTOCOL		; R (12) verify kernel protocol version
;;;	%define CHECKLOADEDHI		; R (11) verify kernel to be loaded high
	%define	ERRORCODES		; R(16) show single letter error codes
	%define	PROGRESS		; R(30) show loader progress on console
	%define KERNELVERSIONSTRING	; R(20) show kernel version string
;;;	%define	ONETIMEBOOT		; R(53) support ELL_flags one-time boot
	%define	A20CHECK		; R(24) verify A20 is enabled
;;;	%define	A20CHECKDELAY		; R delay before verifying A20 enabled
;;;	%define	A20BYKEYBOARD		; ?(45) oldest and biggest A20 enabler
;;;	%define	A20BY0XEE		; ? smallest A20 enabler, rarely works.
	%define A20BYFASTGATESMALL	; ?
;;;	%define A20BYFASTGATESAFE	; ? bigger than above, but also checks
;;;	%define	A20BYINT15		; ? BIOS A20 enabler
;;;	%define	A20CHECKEACH		; R check A20 after each set attempt
;;;	%define BLOCKS64		; D(0) in case 128 doesn't work for you.
;;; 	diagnostics:
;;;	%define	DUMP			; D marginally useful subroutine
;;;	%define	HEXPRINT		; D(32) adds print_dx_hex subroutine
;;;	%define	CHECKBIOSEXT		; D() check BIOS int13 ah=4x support
;;;	%define	CHECKBIOSCHSLIMITS	; D() hack brute force limits discovery
 	
;;; if you know you're loading a modern kernel, you can safely undefine
;;; both CHECKPROTOCOL and CHECKLOADEDHI
	
	%define ORIGIN	0x7c00	;where the BIOS loads us to
	%define	STACK	ORIGIN	;also where we put the stack pointer
	%define	MBRSIG	510	;offset into the MBR of it's word signature
	%define	PPT	446	;offset into the MBR of the PPT
	%define MBREND	PPT	;THIS IS OUR COMPLETE CODESPACE
	
;;; ============================================================
;;; don't mess with these, they define code block dependencies
;;; ============================================================

	%ifnidn	SIGSIZE, 4
	 %ifnidn SIGSIZE, 2
	  %ifnidn SIGSIZE, 1
	   %error "SIGSIZE must be 1, 2 or 4 only"
	  %endif
	 %endif
	%endif
	
	%undef	PRINTAL		;(8)
	%undef	PRINTSTRING	;(18)
	%undef	A20SET

	%ifdef	CMDLINE
	 %ifdef	CMDLINEPPT
	  %error only one of "CMDLINE" and "CMDLINEPPT" can be defined
	 %endif	;CMDLINEPPT
	%endif	;CMDLINE

	%ifdef	A20CHECKDELAY
	 %ifndef A20CHECK
	  %error "A20CHECKDELAY" has no effect without "A20CHECK"
	 %endif	;A20CHECK
	%endif	;A20CHECKDELAY

	%ifdef	A20BYKEYBOARD
	 %define A20SET
	%endif	;A20BYKEYBOARD
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
	 %undef	A20SET
	 %undef	A20BYKEYBOARD
	 %undef	A20CHECK
	 %undef	A20CHECKDELAY
	 %undef	A20CHECKEACH
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
	[map all ELL.map]

	org	ORIGIN

	cli		; dog do we need this?
	mov	sp, STACK	; setup stack
	xor	ax, ax
	mov	ss, ax
	mov	ds, ax
	cld			; do we need this.. maybe..?
;;; save the boot device number passed from the BIOS in dl
	push	dx	; we read it in the stack at 0:0x7bfe (svd3byts)
	
;;; Read Drive Parameters (ah=0x08) to get the sectors per heads per cylinder
	mov	ah, 0x08
	int	0x13
	and	cl, 0x3f
 	inc	dh		;heads per cyl
	mov	ch, dh
	push	cx   ;read them in the stack at 0:0x7bc and 0:0x7bd (svd3byts)

%ifdef	PROGRESS
	mov	al, OURNAME1	; the boot loader has started
	call	print_al
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

	mov	dl, [secs_per_head]
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

%ifdef	A20SET		; if needed, we will use at least one method to enable A20
 %ifdef	A20CHECK	; we will check first if it's already enabled
	call	check_a20_no_delay
	jne	a20_is_enabled
 %endif	;A20CHECK

 %ifdef	A20BYKEYBOARD
a20bykeyboard:
;;;  can we trash the whole disable/enable keyboard part?
;;; don't press keys while booting ;-)
;;;	cli			;dog
;;;	mov	ah, 0xAD	;disable keyboard
;;;	call	a20kbout
	mov	ah, 0xD0	;read from input
	call	a20kbout
.wait:
	in	al, 0x64
	test	al, 1
	jz	short .wait

	in	al, 0x60
	push	ax
	mov	ah, 0xD1	;write to output
	call	a20kbout
	pop	ax
	or	al, 2
	out	0x60, al	;enable A20

;;;	mov	ah, 0xAE	;enable keyboard
;;;	call	a20kbout
;;;	sti			;dog

  %ifdef A20CHECKEACH
	call	check_a20
	jne	short a20_is_enabled
  %endif ;A20CHECKEACH
	%ifdef	REPORTSIZES
	%assign	A20BYKEYBOARDSIZE $-a20bykeyboard
	%endif	;REPORTSIZES
 %endif	;A20BYKEYBOARD

 %ifdef	A20BY0XEE		;this method rarely works
	.a20by0xee:
;;; the "0xee" method (read/write strobe to 0xee) (al content irrelevent)
;;;	out	0xef, al		;RESET (for information only)
	in	al, 0xee		;Enable A20
;;;	out	0xee, al		;Disable A20 (for information only)
  %ifdef A20CHECKEACH
	call	check_a20
	jne	short a20_is_enabled
  %endif ;A20CHECKEACH
	%ifdef	REPORTSIZES
	%assign	size $-.a20by0xee
	%warning "A20BY0XEE" added size bytes
	%endif	;REPORTSIZES
 %endif	;A20BY0XEE

 %ifdef A20BYFASTGATESMALL
;;; the "fast A20 gate" method (io data at 0x92)
;;; quick and dirty version
;;; note: bit 0 of 0x92 is RESET
 	in	al, 0x92
 	or	al, 2
	out	0x92, al	;Enable A20
  %ifdef A20CHECKEACH
	call	check_a20
	jne	short a20_is_enabled
  %endif ;A20CHECKEACH	
 %endif  ;A20FASTGATESMALL
 %ifdef	A20BYFASTGATESAFE
;;; safer use of fast A20 gate
	in	al, 0x92
	test	al, 2		;A20 already enabled?
	jnz	short .fastgate_bit2

	or	al, 2		;set A20 enable
	and	al, 0xFE	;unset RESET
	out	0x92, al	;enable A20
.fastgate_bit2:
  %ifdef A20CHECKEACH
	call	check_a20
	jne	short a20_is_enabled
  %endif ;A20CHECKEACH	
 %endif	;A20BYFASTGATESAFE

 %ifdef	A20BYINT15
;;; BIOS int15 method
;;;	mov	ax,0x2403	;first check if supported.
;;;	int	0x15		;maybe this can be left out?
;;;  %ifdef ERRORCODES
;;;	mov	al, INT15FAILED
;;;  %endif ;ERRORCODES
;;;	jb	err_al	;int15 unsupported?
;;;
;;;	cmp	ah, 0
;;;	jnz	err_al

	mov	ax, 0x2401 ; A20 line enable via BIOS
	int	0x15
 %endif ;A20BYINT15
%endif	;A20SET

%ifdef A20CHECK		;did A20 finally get enabled?
	call	check_a20
	je	err_al
%endif ;A20CHECK

;;;---------------------------------------------------------------------
;;;	A20 IS ENABLED		ds=0x0000 es=0 *if* no check_a20 calls
;;;---------------------------------------------------------------------
a20_is_enabled:
	%ifdef	A20CHECK      ;check_a20 crashed es
	lgdt	[ds:gdt_desc]	;load the global/interupt descriptor table
	%else
	lgdt	[gdt_desc]	;load the global/interupt descriptor table
	%endif ;A20CHECK

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
	sti			; finally dog do I need this?

;;;---------------------------------------------------
;;;	now in UNREAL MODE		ds=es=0x0000
;;;---------------------------------------------------

	%ifdef	PROGRESS
	mov	al, OURNAME2	; A20 and UNREAL achieved
	call	print_al
	%endif	;PROGRESS

;;; load the MBR, and GPT (LBA0, LBA1, and LBA2)
	xor	ax, ax		; starting with LBA=0
	mov	dl, 3		; three sectors (LBA0-LBA2)
	mov	bx, MBR		; offset (in es which is 0x0000)
	call	disk_read	; load first three sectors into 0:MBR
	%ifdef	PROGRESS
	mov	al, OURNAME3	; GPT loaded
	call	print_al
	%endif	;PROGRESS

%ifdef	CHECKGPTSIG
	.CHECKGPTSIG:
%ifdef	ERRORCODES
	mov	al, GPTSIGERR
%endif	;ERRORCODES
	%ifidn	 SIGSIZE, 4
 	cmp	dword [GPT_sig], 'EFI '	;GPT signature
	%elifidn SIGSIZE, 2
	cmp	word [GPT_sig+2], 'I '	;GPT sig, save three bytes
	%else
	cmp	byte [GPT_sig], 'E'	;GPT sig, save four bytes
	%endif
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

	push	ax		; save ELL_flags
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
 	jnc	.write_ok	; should this be an error? we could still boot.
	call	print_al	;this error is not fatal, dog should it be?
.write_ok:
	pop	ax		; restore ELL_flags
;;;	mov	al, byte [MBR_ELL_flags] ; push/pop is one byte smaller.
	xor	al, 00000001b	;toggle the default kernel partition

.default_partition:
	test	al, 00000001b	;which partition to load kernel from
	%ifdef PROGRESS
	mov	al, PART1
	%endif ;PROGRESS
	jz	.part1

	mov	ax, [part2_FLBA] ;change default by copying partition 2 to 1
	mov	[part1_FLBA], ax
	%ifdef	CMDLINE
	inc	byte [cmdLine+cmdLineLen-2] ;use the second root filesystem
	%endif
	%ifdef	CMDLINEPPT
	inc	byte [command_line_end-1]
	%endif
	%ifdef PROGRESS
	mov	al, PART2
	%endif ;PROGRESS

.part1:
	%ifdef PROGRESS
	call	print_al
	%endif ;PROGRESS
	%ifdef	REPORTSIZES
	%assign	size $-.ONETIMEBOOT
	%warning "ONETIMEBOOT" added size bytes
	%endif	;REPORTSIZES
%endif	;ONETIMEBOOT

;;; load the kernel header and real mode code to 0x1000:0

	push	0x1000
	pop	es		; es=0x1000 ds=0x0000
	mov	ax, [part1_FLBA] ; first LBA of partition one
	mov	dl, 64		;maximum kernel header size of 32K (64 sectors)
	call	disk_read0	;load the first 32K block of the kernel

%ifdef	PROGRESS
	mov	al, OURNAME4	; kernel realmode code loaded
	call	print_al
%endif	;PROGRESS

	push	ds		;0x0000
	push	es		;0x1000
	pop	ds		; ds = es = 0x1000, 0x0000 on stack

%ifdef	CHECKKERNELSIG
	.CHECKKERNELSIG:
	%ifidn	 SIGSIZE, 4
	cmp	dword [0x202], 'HdrS' ;signature
	%elifidn SIGSIZE, 2
	cmp	word [0x202+2], 'rS' ;signature, save three bytes
	%else
	cmp	byte [0x202], 'H' ;signature, save four bytes
	%endif
	%ifdef	ERRORCODES
	mov	al, KSIGERR
	%endif	;ERRORCODES
	jne	err_al
	%ifdef	REPORTSIZES
	%assign	size $-.CHECKKERNELSIG
	%warning "CHECKKERNELSIG" added size bytes
	%endif	;REPORTSIZES
%endif	;CHECKKERNELSIG

%ifdef	CHECKPROTOCOL
	cmp	word [0x206], 0x204	; kernel boot protocol version
	%ifdef	ERRORCODES
	mov	al, KPROTOERR
	%endif	;ERRORCODES
	jb	err_al			; must be protocol v2.04 or greater
%endif	;CHECKPROTOCOL
	
%ifdef	CHECKLOADEDHI
	%ifdef	ERRORCODES
	mov	al, KLOADHIERR
	%endif	;ERRORCODES
	test	byte [0x211], 1 	; loadflags: boot protocol option flags
 	jz	err_al			; error if not loaded high: 0x100000
%endif	;CHECKLOADEDHI

%ifdef	KERNELVERSIONSTRING
	.KERNELVERSIONSTRING:
	mov	si, [0x20e]	; pointer to kernel version string
	add	si, 0x200	; skip over the kernel boot sector
	call	print_string	;print the kernel version string
	%ifdef	REPORTSIZES
	%assign	size $-.KERNELVERSIONSTRING
	%warning "KERNELVERSIONSTRING" added size bytes
	%endif	;REPORTSIZES
%endif	;KERNELVERSIONSTRING
;;;
;;; 	obligatory kernel setup ( ds = es = 0x1000, and 0x0000 on stack )
;;;
%ifdef	ERRORCODES
	mov	word [0x210], 0x81e1	; set LOADHI+USEHEAP+type_of_loader
%else
	;; set kernel quiet mode if ERRORCODES not enabled
	mov	word [0x210], 0xa1e1	; set LOADHI+USEHEAP+type_of_loader
%endif	;ERRORCODES

;;; 	if you want to play with initrd, do it here:
;;;	mov	dword [0x218], 0x0000 ;set ramdisk_image
;;;	mov	dword [0x21C], 0x0000 ;set ramdisk_size

	mov	word [0x224], 0xde00	 ;set heap_end_ptr
;;;	mov	byte [0x226], 0x00	 ;set ext_loader_ver
	mov	byte [0x227], 0x01	 ;set ext_loader_type / bootloader id 11
	%ifdef	CMDLINEPPT
 	mov	dword [0x228], command_line ; set cmd_line_ptr
	%else	;CMDLINEPPT
 	mov	dword [0x228], cmdLine ; set cmd_line_ptr
	%endif	;CMDLINEPPT
	pop	ds		       ; ds=0x0000 es=0x1000(kernel realmode segment)

    ; the protected mode part must be loaded at 0x100000
    ; load 128or64 sectors at a time to 0x2000, then copy to 0x100000

;;; load the kernel protected mode code to 0x100000

 	mov 	al, [es:0x1f1] ; no of blocks in the kernel realmode header
 	or	al, al
 	jnz	short has_size

	mov	al, 4		;default size

has_size:
	xor	ah,ah ; can I assume ah=0? set last by bios read status maybe?
	add	ax, [part1_FLBA] ;compute first LBA of protected mode code
	inc	ax	;plus one for the legacy boot sector, ax=LBA to load

 	mov	ecx, [es:0x1f4] ;num of 16byte pages of protected code to load
;;; /32 (pages per block) to get blocks, then by 128 to get 128 block groups
	%ifdef	BLOCKS64
	shr	ecx, 11		; /2048 = number of 64block groups to load
	%else
 	shr	ecx, 12		; /4096 = number of 128block groups to load
	%endif	;BLOCKS64

	inc	cx		; +1 potentially partial group

;;;	the following can be pulled out as a subroutine should you wish
;;;	to call it more than once.
;;;
;;; 	call	load_high	 ;
;;;------------------------------------------------------------------------
;;; On Entry:	ax=starting LBA, cx=number of groups to move
;;;		[highmove_addr]=destination
;;; 
;;; load a group (128or64) of logical blocks, starting from LBA=ax
;;; into the buffer at 0x2000, then copy them to [highmove_addr]
;;; repeat for cx number of block groups
;;;------------------------------------------------------------------------

load_high:
	push	0x2000		 ;buffer segment to load block groups into
	pop	es		 ;es=0x2000 ds=0x0000

.loadloop:
	%ifdef	BLOCKS64
	mov	dl, 64 ;blocks per group
	%else
	mov	dl, 128 ;blocks per group
	%endif	;BLOCKS64
	push	cx	;save the group count
	push	ax     ;save the current LBA
	call	disk_read0	;read 128or64 blocks into disk read buffer

	mov	esi, 0x20000	;the disk read buffer segment
	mov	edi, dword [highmove_addr]
	%ifdef	BLOCKS64
	mov	cx, 512*64/4
	%else
	mov	cx, 512*128/4
	%endif	;BLOCKS64
	a32	rep movsd	;move read buffer to high memory

	mov	dword [highmove_addr], edi
%ifdef	PROGRESS
	mov	al, '.'
	call	print_al	;report the progress
%endif	;PROGRESS
	pop	ax		; restore the LBA
	pop	cx		; restore the group count
	%ifdef	BLOCKS64
	add	ax, 64		; next LBA to load from
	%else
	add	ax, 128		; next LBA to load from
	%endif	;BLOCKS64
	dec	cx
	jnz	short .loadloop

;;; 	ret		; end of load_high subroutine (here inlined)
;;;	-----

	%ifdef	PROGRESS
	mov	al, KLAUNCH	; starting the Kernel
	call	print_al
	%endif	;PROGRESS

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
	call	print_al
	%else
	 %ifdef PROGRESS
	mov	al, '!'		;if PROGRESS, then print_al, so show anon err
	call	print_al
	 %endif	;PROGRESS
	%endif 	;ERRORCODES
err:	jmp	short $		;hang forever
;;	---------------

;;;-------------------------------------------------------------------
;;;
;;; READ UP TO 128 LOGICAL BLOCKS USING BIOS INT13 AH=0x02 Cyl/Hds/Sec
;;;
;;; on entry:
;;; ax=FirstLBA dl=CountOfBlocksToRead es:bx=DestinationAddress
;;; !! assumes ds=0x0000 !!
;;; crashes: calls BIOS, so who knows... should preserve segment regs
;;;
;;; with a 16bit LBA address, we get to the first 33.5MB of HDD
;;; ( (2**16)*512 = 33554432 ) which should be enough for two kernels,
;;; one in each of the first two partitions
;;;
;;; honors BIOS drive parameters for secs/hd and hds/cyl
;;;
;;;-------------------------------------------------------------------
disk_read0:
	xor	bx, bx		;null offset into destination segment
disk_read:
;;; convert LBA in dx to c/h/s in ch/dh/cl

	div	byte [secs_per_head]
	xor	cx, cx
	mov	cl, ah
 	inc	cl		;sectors start with 1
	xor	ah, ah
	div	byte [hds_per_cyl]
	mov	dh, ah		;head
	mov	ch, al		;cyl
	mov	al, dl		;number of sectors to read

;;; now ch=cyl, dh=head, cl=sector (bits 6+7 are 0)

	mov	ah, 0x02	;read drive sectors command
	mov	dl, [boot_drive]
	int	0x13
	%ifdef	ERRORCODES
	mov	al, HDDERR
	%endif	;ERRORCODES
	jc	short err_al

return:
	ret
;;;	---

;;;---------------------------------------------------------
	
%ifdef PRINTSTRING
;;;---------------------------------------------------------
;;; on entry ds:si points to NULL terminated string to print
;;;---------------------------------------------------------
print_string_loop:	
	call	print_al	; 3

print_string:
	lodsb			;+1
	or	al, al		;+2
	jnz	short print_string_loop	;+2

	ret			;+1=9
;;; 	---
	%ifdef	REPORTSIZES
	%assign	size $-print_string_loop
	%warning "PRINTSTRING" added size bytes
	%endif	;REPORTSIZES
%endif ;PRINTSTRING
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
	mov	ah, 0xe		; 2
	mov	bx, 7		;+3
	int	0x10		;+2
	ret			;+1=8
;;;	---
	%ifdef	REPORTSIZES
	 %assign	size $-print_al
	 %warning "PRINTAL" added size bytes
	%endif	;REPORTSIZES
%endif	;PRINTAL		;MUST FOLLOW PRINTHEX
	
;;;------------------------------------------------
	
%ifdef DUMP
dump:
	push	0x1000
	pop	ds
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
	
%ifdef A20CHECK
;;; upon return, will jne if A20 is enabled, je if A20 is likely disabled
;;; crashes ax, es, and maybe cx
check_a20:
 %ifdef A20CHECKDELAY
;;; wait to see if it eventually enables
 	xor	cx, cx	; arbitrary delay loop value
.delay_loop:
	call	check_a20
	jne	short return

	dec	cx
	jnz	short .delay_loop
 %endif ;A20CHECKDELAY
check_a20_no_delay:
	xor	ax, ax
 	dec	ax		; 1byte smaller than 'not'
	mov	es, ax		; 0xffff
	cmp	word [es:0x7e0e], 0xaa55 ; boot sector ID mirrored in himem?
 %ifdef ERRORCODES
	mov	al, A20ERR
 %endif ;ERRORCODES
	ret
;;;	---

 %ifdef UNDEFINED		;another method, more sure, too big.
	xor	ax, ax
;;;	mov	ds, ax
	dec	ax ; ax = 0xFFFF
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
	%assign	size $-check_a20
	%warning "A20CHECK" added size bytes
	%endif	;REPORTSIZES
%endif	;A20CHECK

;;;----------------------------------------------------------------
;;;	    interface to keyboard controller to enable A20
;;; caution: this can hang on systems without a keyboard controller
;;;----------------------------------------------------------------
	%ifdef	A20BYKEYBOARD
a20kbout:
.wait:
        in      al, 0x64
        test    al, 2
        jnz     short .wait

	mov	al, ah
	out     0x64, al
        ret
;;;	---
	%ifdef	REPORTSIZES
	 %assign size ($-a20kbout)+A20BYKEYBOARDSIZE
	  %warning "A20BYKEYBOARD" added size bytes
	 %endif	;REPORTSIZES
	%endif	;A20BYKEYBOARD

;;;-------------------------------------------------
;;;		start of initialized data
;;;-------------------------------------------------

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

;;;	---------------------

highmove_addr:	dd	0x100000 ;pointer to protected mode code destination

;;;	---------------------

	%ifdef	CMDLINE
cmdLine:
	db	CMDLINE
	db	0
cmdLineLen:	equ	$-cmdLine
	%endif

;;; ---------------------------------------------------
;;;
;;; Assuming that the object code is copied to the boot disk using
;;; "dd if=disk of=/dev/sd?", and that said disk was previously
;;; partitioned as GPT, then a "protective partition table"
;;; and the 0xaa55 signature are already there. Therefore, we
;;; stop at MBREND to avoid overwriting the "PPT"
;;;
;;; without the 0xaa55 we don't really need this, but it may "initialize"
;;; some of the uninitialized data to zero, looks cleaner on disk, and
;;; very helpfully generates assembly errors when we have too much code.

	%assign REALELLFLAGS MBREND-1
	%assign	freecodespace	MBREND-($-$$)-1

	%ifdef	ONETIMEBOOT
	 %ifdef	CMDLINEPPT
	  %warning "ELL_flags" is at REALELLFLAGS, "COMMANDLINE" starts at CMDLINEPPT, freecodespace bytes available
	 %else
	  %warning "ELL_flags" is at REALELLFLAGS, freecodespace bytes available
	 %endif
	%else
	 %ifdef CMDLINEPPT
	  %warning "COMMANDLINE" starts at CMDLINEPPT, freecodespace bytes available
	 %else
	  %warning freecodespace bytes available
	 %endif
	%endif	;ONETIMEBOOT
 	times	MBREND-($-$$)-1	db	0 ;the -1 is for ELL_flags
;;; --------------------------------------------------
;;; The ELL_flags byte is in LBA0 at offset MBREND-1
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

;;; We load three sectors, LBA0-LBA2 to this addres in segment 0x0000
;;; this then, is an actual copy of the disk MBR, and GPT

MBR:
	times	MBREND-1	resb	1
;;; This is the ELL flag byte on disk
MBR_ELL_flags:	resb	1

	%ifdef	CMDLINEPPT
	times	CMDLINEPPT-($-MBR)	resb	1 ;push to CMDLINEPPT
command_line:	times MBRSIG-($-MBR)-1	resb	1
command_line_end:	resb	1 		;(the terminating NULL)
	command_line_len	equ	$-command_line
	%endif

	times	0x200-($-MBR)	resb	1 ; bump to LBA1
	
;;; the GPT (GUID Partition Table) header (LBA1)
GPT_header:
GPT_sig:
	times	(2*0x200)-($-MBR)	resb	1 ; bump to LBA2

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
	times	(2*0x80)+0x20-($-GPT_entries)	resb	1
part3_FLBA:	resw	1	;GPT part2 First LBA
	times	6	resb	1
part3_LLBA:	resw	1	;GPT part2 Last LBA
;;; 	times	(3*0x200)-($-MBR)	resb	1 ; bump past end of LBA2

;;; these three provided by BIOS,

;;; to save code space these are pushed on the stack early
;;; and then read in place on the stack when needed
;;; (push instructions are usually small, so we saved six bytes)

	absolute STACK - 4
secs_per_head:		;BIOS provided boot disk parameter
	resb	1
hds_per_cyl:		;BIOS provided boot disk parameter
	resb	1
boot_drive:		;BIOS provided boot disk device number
	resb	1
	resb	1
