ELL: Embedded Linux Loader
==========================

An x86 single sector Linux bootloader.

It does not support initrd, and the kernel command line is limited to 48 bytes.

Uses ah=0x02 for int13 disk reads (my olden boxen won't do 0x42)

This linux bootloader coexists with the Protective Partition Table (PPT)
of a GUID (GPT) Partitioned disk, entirely within the first sector of the
boot disk.

It expects a valid GUID Partition Table (GPT), with two valid linux
kernels in the first two partitions (no filesystems).
All of both partitions must be within the fisrt 33.5MB of the disk,
as we only use a 16bit logical block address (LBA).

A byte at offset 447(0x1bf) in the first sector contains two flag bits
declaring which kernel will boot by default, and whether to boot
the other kernel once, clearing that flag.

The kernels in partitions 1 and 2 are presumed to mount the filesystems
in partitions 3 and 4 respectively.

The project here is to boot a kernel in partition 1, mounting
partition 3 as root cramfs, or the kernel in partition 2, mounting
partition 4 as root cramfs.

This gives us two complete system copies.

This allows us to upgrade the kernel and root filesystem,
(via busybox "dd" of the kernel and cramfs into two partitions)
while keeping the older stable system should the new one fail.

Resources permitting, we then mount partition 5 as SWAP, and
partion 6 as overlayfs.

With a watchdog started by the kernel, and tickled by the OS
only when it believes it is stable (and remotely accessible),
the result should be safely upgradable remotely via dropbear.

This code assembles using NASM.

The command line passed to the kernel can be defined inline,
severely limiting the size, or it can be placed in the first
sector of the boot drive at offset 0x1cd through 0x1fd, partially
crashing the end of the Protective Partition Table. The last
character in the command line is bytewise incremented when
booting the second kernel partition, so the root= command must
be last.

The embedded system itself can change which kernel will be booted
by default, the command line, and whether the other kernel should
be booted only once, all using busybox "dd" or "hexedit".

Various modules for reporting boot progress, errors, checking and
setting A20, and some diagnostics, can be enabled or disabled
individually to trim code to fit in the limited space.

To build, you need to:

1. Create a GPT partition on your boot media. I use fdisk which
   allows triming the table size and micromanaging partition
   boundaries to get the most use of minimal media often used
   in embedded systems. I use 6 partitions. The first two contain
   raw kernel images. The next two contain cramfs images, but could
   be any filesystem supported by the kernels. Then a SWAP and
   overlayfs partitions.

2. Edit the source to your liking, selecting desired features.
   (by commenting out and uncommenting out %define statements)

3. Assemble with "nasm -o mbr ELL.asm"

4. Go back to step 2, until it all assembles within the tiny space.

5. "dd" the mbr image directly to the boot media:

   "dd if=mbr of=/dev/sd?"

6. Using hexedit (carefully) or scripting "dd" magic on the boot disk,
   insert your kernel command line to the 48 bytes at offset 0x1cd.
   All 48 must contain text, and the 48th character will be bytewise
   incremented when booting from the second kernel partition, so the
   "root=" command must be last. The 49th byte (at 0x1fd) is the NULL
   string terminator, follwed by the 0x55aa signature at 0x1fe.

   "echo '\"                                  root=/dev/sd?3' | dd of=/dev/sd? seek=461 bs=1  count=48"
   (NOTE: that is exactly 48 alphanumeric characters between the single quotes above)
   
   "dd if=/dev/zero of=/dev/sd? bs=1 count=1 seek=509"

7. The media should now be bootable.

I am using ELL here for multiple embedded routers etc., and am quite
happy with the results, but this is VERY MUCH ALPHA CODE.

