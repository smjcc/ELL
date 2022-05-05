ELL: Embedded Linux Loader
==========================

An x86 single sector Linux bootloader.

Uses ah=0x02 for int13 disk reads, so it does not require the
ah=0x42 int13 extensions.

This Linux bootloader coexists with the Protective Partition Table (PPT)
of a GUID (GPT) Partitioned disk, entirely within the first sector of the
boot disk.

It expects a valid GUID Partition Table (GPT), with one or two valid Linux
kernels in the first one or two partitions (no filesystems).

If ONETIME is defined, a flag byte in the sector contains two flag bits
declaring which of two kernels will boot by default, and whether to boot
the other kernel once, clearing that flag. (sector must be written to)

The kernels in partitions 1 and 2 are then presumed to mount the filesystems
in partitions 3 and 4 respectively, so the last byte of the kernel command
string is incremented when the second kernel is loaded.
(e.g. root=sda3 must be right justified at the end of the command buffer)

LIMITATIONS:

This code assembles using NASM.

It does not support loading an initrd.
(you can still use the kernel's internal initramfs)

The kernel command line is limited to 47 bytes, or consumes code space.

All kernel partitions must be within the first 33.5MB of the disk,
as a 16bit logical block address (LBA) is used.

The command line passed to the kernel can be stored below or inside
of the protective partition table. When stored below, it consumes space
otherwise available for code. When stored inside, it crashes the last
three partition entries. (fdisk will report this as a "hybrid" GPT,
but the kernel will use the GPT data, and no other problems have been
seen with this approach so far)

THEORY:

The project here is to boot a kernel in partition 1, mounting
partition 3 as root squashfs, or the kernel in partition 2, mounting
partition 4 as root squashfs.

This gives us two complete copies of the operating system, which allows
us to upgrade the kernel and root filesystem, while retaining a backup
to fall back to should the upgrade fail.

With a watchdog started by the kernel, and tickled by the OS
only when it believes it is stable (and remotely accessible),
the result should be safely upgradable remotely via dropbear.

The embedded system itself can change which kernel will be booted
by default, the command line, and whether the other kernel should
be booted only once, all using busybox "dd" or "hexedit".

Various blocks of code for reporting boot progress, errors, checking
and setting A20, and some diagnostics, can be enabled or disabled
individually to trim code to fit in the limited space.

To build, you need to:

1. Create a GPT partition on your boot media. I use fdisk from
   util-linux which allows trimming the table size and micromanaging
   partition boundaries.

2. Edit the source to your liking, selecting desired features.
   (by commenting out and uncommenting out %define statements)

3. Assemble with "nasm -o mbr ELL.asm"

4. Go back to step 2, until it all assembles within the tiny space.

5. "dd" the mbr image directly to the boot media:

   "dd if=mbr of=/dev/sd?"

6. Using hexedit (carefully) or scripting "dd" magic on the boot disk,
   insert your kernel command line to the 47 bytes starting at offset
   462 (0x1ce). All 47 must contain text, and the 47th character will
   be byte-wise incremented when booting from the second kernel partition,
   so the "root=" command must be last and right justified. The 48th byte
   (at 0x1fd) must be the NULL (0) string terminator, followed by the
   0x55, 0xaa signature at 0x1fe, and 0x1ff.

7. The media should now be bootable.

I am using ELL here for multiple embedded routers etc., and am quite
happy with the results, but this is VERY MUCH ALPHA CODE.

