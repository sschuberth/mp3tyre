**mp3TYRE-10.2007.aug.24**
  * CHG: ADD: Added support for detecting and correcting invalid padding inside ID3v2 tags.
  * CHG: ADD: Added explicit support for ID3v1 tags (which were previously handled but not
    reported).
  * CHG: ADD: The executable now has an icon.
  * CHG: mp3TYRE is now hosted as an Open Source project on Google Code.
  * CHG: Changed compiler to most recent Free Pascal / Lazarus accompanied by several
    code clean-ups.
  * CHG: Look for the mp3PRO plug-in also in Winamp's plug-in directory.
  * CHG: Reworked and added more status messages.
  * CHG: Simulation now also affects cleaning, not only renaming.

**mp3TYRE-9.2006.feb.19**
  * CHG: ADD: Optionally write out a cleaned MP3 file.
  * CHG: Also search for the mp3PRO plug-in in the local directory.
  * CHG: Reformatted some output messages

**mp3TYRE-8.2006.jan.15**
  * CHG: Made mp3TYRE compile with Free Pascal 2.0.2.

**mp3TYRE-7.2005.nov.30**
  * CHG: Changed some information printed out to screen

**mp3TYRE-6.2004.jan.09**
  * CHG: More space-saving statistics layout
  * CHG: Minor speed improvements
  * CHG: Source code clean-up
  * FIX: Support for VBR mp3PRO files

**mp3TYRE-5.2003.jul.13**
  * CHG: ADD: Command line switch to enable printing of full paths
  * CHG: ADD: Skipping of junk data (e.g. broken MP3 frames) at the beginning of files
  * CHG: ADD: Added support for MPEG 2.5 audio frames
  * CHG: More detailed analyzing information and improved statistics
  * CHG: Better handling of corrupted files
  * CHG: Lots of internal changes
  * FIX: Analyzing files which contain special characters (e.g. "[") in their names

**mp3TYRE-4.2003.jul.09**
  * FIX: Processing of files smaller than 64kb

**mp3TYRE-3.2003.mai.06**
  * CHG: ADD: Default file mask is "*.mp3"
  * CHG: ADD: Command line switches to separately include / exclude processing of CBR, VBR
    and mp3PRO files
  * CHG: Source code clean-up
  * CHG: Cosmetic improvements
  * CHG: Simulation mode is enabled by default
  * FIX: Command line parsing of directories

**mp3TYRE-2.2003.apr.24**
  * FIX: Recognize MP3 files with padding between ID3v2 tag and first frame
  * FIX: Recognize MP3 files with RIFF / Wave header before first frame

**mp3TYRE-1.2002.aug.27**
  * CHG: ADD: First public version

_Legend: ADD = Added, CHG = Changed, FIX = Fixed_
