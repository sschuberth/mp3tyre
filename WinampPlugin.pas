{
    Winamp 2.x input / output plug-in definitions

    This is merely a Pascal version of the "in_minisdk.zip" example code which
    is still available at:

    <http://nunzioweb.com/sawgstuff/mirror/nullsoft/in_minisdk.zip>
}

unit WinampPlugin;

interface

uses
    Windows;

type
    OutModulePtr=^OutModule;
    OutModule=record
        Version:Integer;   // Module version (OUT_VER).
        Description:PChar; // Description of module with version string.
        ID:Integer;        // Each input module gets its own ID. Non-Nullsoft modules should be >= 65536.

        hMainWindow:HWND;   // Winamp's main window (filled in by Winamp).
        hDLLInstance:HINST; // DLL instance handle (filled in by Winamp).

        Config:procedure(hWndParent:HWND); cdecl; // Configuration dialog window.
        About:procedure(hWndParent:HWND); cdecl;  // About dialog window.

        Init:procedure; cdecl; // Called when loaded.
        Quit:procedure; cdecl; // Called when unloaded.

        Open:function(SampleRate,NumChannels,BitsPerSample,BufferLenMS,PreBufferMS:Integer):Integer; cdecl;
        // Returns the maximum latency in milliseconds (0 for diskwriters etc., <0 on failure).
        // NOTE: "BufferLenMS" and "PreBufferMS" are ignored in most if not all output plug-ins, so
        // don't expect the maximum latency returned to be what you asked for.
        // "PreBufferMS" must be <= "BufferLenMS", specify 0 to use defaults.

        Close:procedure; cdecl; // Close the output device.

        Write:function(Buffer:PChar;Length:Integer):Integer; cdecl;
        // Returns 0 on success, 1 if not able to write (yet). Non-blocking, always.
        // "Length" specifies the number of bytes to write (always <= 8192).
        // "Buffer" points straight into the audio data.

        CanWrite:function:Integer; cdecl;
        // Returns the number of bytes possible to write at a given time.
        // Never will decrease unless you call "Write" (or "Close").

        IsPlaying:function:LongBool; cdecl;
        // Non-zero if output is still going on or if data in buffers waiting to be
        // written (i.e. closing while "IsPlaying" returns 1 would truncate the song).

        Pause:function(Pause:Integer):Integer; cdecl; // Returns the previous pause state.

        SetVolume:procedure(Volume:Integer); cdecl; // Volume is in range 0 to 255.
        SetPan:procedure(Pan:Integer); cdecl;       // Pan is in range -128 to 128.

        Flush:procedure(Time:Integer); cdecl;
        // Used for seeking, flushes buffers and restarts output at the given time (in ms).

        GetOutputTime:function:Integer; cdecl;
        // Returns the played time in MS.

        GetWrittenTime:function:Integer; cdecl;
        // Returns the time written in MS (used for synchronizing up visualization stuff).
    end;

    EqualizerData=array[0..9] of Char;

    InModulePtr=^InModule;
    InModule=record
        Version:Integer;   // Module type (IN_VER).
        Description:PChar; // Description of module with version string.

        hMainWindow:HWND;   // Winamp's main window (filled in by Winamp).
        hDLLInstance:HINST; // DLL instance handle (also filled in by Winamp).

        FileExtensions:PChar;
        // Format "mp3\0Layer 3 MPEG\0mp2\0Layer 2 MPEG\0mpg\0Layer 1 MPEG\0",
        // may be altered from config, so users can select what they want.

        IsSeekable:LongBool;    // Is this stream seekable?
        UsesOutputPlug:Integer; // Does this plug-in use the output plug-ins (must not change)?

        ConfigDialog:procedure(hWndParent:HWND); cdecl; // Configuration dialog window.
        AboutDialog:procedure(hWndParent:HWND); cdecl;  // About dialog window.

        Init:procedure; cdecl; // Called at program initialization.
        Quit:procedure; cdecl; // Called at program quit.

        // If FileName is NIL, the currently playing file is used.
        GetFileInfo:procedure(FileName,Title:PChar;var LengthInMS:Integer); cdecl;
        InfoBox:function(FileName:PChar;hWndParent:HWND):Integer; cdecl;

        IsOurFile:function(FileName:PChar):LongBool; cdecl;
        // Called before extension checks to allow detection of "mms://" etc.

        { Playback stuff }

        Play:function(FileName:PChar):Integer; cdecl;
        // Return 0 on success, -1 on file-not-found and some other value on other error (stopping Winamp).

        Pause:procedure; cdecl; // Pause stream.
        Unpause:procedure; cdecl; // Unpause stream.
        IsPaused:function:LongBool; cdecl; // Is paused? Returns 1 if paused, 0 if not.
        Stop:procedure; cdecl; // Stop (unload) stream.

        { Time stuff }

        GetLength:function:Integer; cdecl;
        // Get length in milliseconds.

        GetOutputTime:function:Integer; cdecl;
        // Returns current output time in milliseconds, usually returns "OutModule.GetOutputTime".

        SetOutputTime:procedure(TimeInMS:Integer); cdecl;
        // Seeks to point in stream. Usually you signal your thread to seek, which
        // seeks and calls "OutModule.Flush".

        { Volume stuff }

        SetVolume:procedure(Volume:Integer); cdecl; // From 0 to 255, usually just calls "OutModule.SetVolume".
        SetPan:procedure(Pan:Integer); cdecl;       // From -127 to 127, usually just calls "OutModule.SetPan".

        { In-window built-in visual stuff }

        SAVSAInit:procedure(MaxLatencyInMS,SampleRate:Integer); cdecl;
        // Called after opening audio device and once in "Play", "MaxLatencyInMS"
        // should be the value returned from "OutModule.Open".

        SAVSADeInit:procedure; cdecl; // Called in "Stop".

        { Simple visualization supplying mode }

        SAAddPCMData:procedure(PCMData:Pointer;NumChannels,BPS,TimeStamp:Integer); cdecl;
        // Sets the spectrum data directly from PCM data, quick and easy way to get
        // visualization working (needs at least 576 samples).

        { Advanced vis supplying mode (use "SAAddPCMData" for most stuff) }

        SAGetMode:function:Integer; cdecl;
        // Gets the current CSA type (4=ws, 2=osc, 1=spec), use when calling "SAAdd".

        SAAdd:procedure(Data:Pointer;TimeStamp,CSA:Integer); cdecl;
        // Sets the spectrum data, filled in by Winamp.

        { Simple visualization supplying mode (plug-in) }

        VSAAddPCMData:procedure(PCMData:Pointer;NumChannels,BPS,TimeStamp:Integer); cdecl;
        // Sets the spectrum data directly from PCM data, quick and easy way to get
        // visualization working (needs at least 576 samples).

        { Advanced visualization supplying mode (plug-in, use "VSAAddPCMData" for most stuff) }

        VSAGetMode:function(var SpectrumChannels,WaveChannels:Integer):Integer; cdecl;
        // Used to figure out what to give to "VSAAdd".

        VSAAdd:procedure(Data:Pointer;TimeStamp:Integer); cdecl;
        // Filled in by Winamp, called by plug-in.

        VSASetInfo:procedure(NumChannels,SampleRate:Integer); cdecl;
        // Call this in "Play" to tell the visualization plug-ins the current output parameters.

        { DSP plug-in processing (filled in by Winamp, called by input plug-in) }

        DSPIsActive:function:LongBool; cdecl;
        // Returns 1 if active (which means that the number of samples returned by DSPDoSamples could
        // be greater than went in. Use it to estimate if you'll have enough room in the output buffer.

        DSPDoSamples:function(var Samples:Shortint;NumSamples,BPS,NumChannels,SampleRate:Integer):Integer; cdecl;
        // Returns the number of samples to output. This can be as much as twice "NumSamples".
        // Be sure to allocate enough buffer for samples, then.

        { Equalizer stuff }

        SetEqualizer:procedure(Enabled:LongBool;Data:EqualizerData;PreAmp:Integer); cdecl;
        // 0-64 each, 31 is +0, 0 is +12, 63 is -12. Do nothing to ignore.

        { Info setting (filled in by Winamp) }

        SetInfo:procedure(BitRate,SampleRate,Stereo:Integer;Synchronized:LongBool); cdecl;
        // If -1, changes are ignored.

        CurrentOutputPlugin:OutModulePtr; // Filled in by Winamp, optionally used.
    end;

var
    OutModuleHandle:HMODULE;
    OutputPlugin:OutModulePtr;
    InModuleHandle:HMODULE;
    InputPlugin:InModulePtr;

function LoadOutModule(FileName:string):Boolean;
procedure FreeOutModule;
function LoadInModule(FileName:string):Boolean;
procedure FreeInModule;

implementation

uses
    SysUtils;

function LoadOutModule(FileName:string):Boolean;
type
    GetOutModuleFunc=function:OutModulePtr; stdcall;
var
    GetOutModule:GetOutModuleFunc;
begin
    GetOutModule:=nil;

    // Free a previously loaded plug-in.
    FreeOutModule;

    // Load the DLL and get the output plug-in structure.
    OutModuleHandle:=SafeLoadLibrary(FileName);
    if OutModuleHandle<>0 then begin
        GetOutModule:=GetOutModuleFunc(GetProcAddress(OutModuleHandle,'winampGetOutModule'));
    end;
    if @GetOutModule<>nil then begin
        OutputPlugin:=GetOutModule;
    end;

    Result:=(OutputPlugin<>nil);
end;

procedure FreeOutModule;
begin
    if OutModuleHandle<>0 then begin
        FreeLibrary(OutModuleHandle);
        OutModuleHandle:=0;
    end;
    OutputPlugin:=nil;
end;

function LoadInModule(FileName:string):Boolean;
type
    GetInModuleFunc=function:InModulePtr; stdcall;
var
    GetInModule:GetInModuleFunc;
begin
    GetInModule:=nil;

    // Free a previously loaded plug-in.
    FreeInModule;

    // Load the DLL and get the input plug-in structure.
    InModuleHandle:=SafeLoadLibrary(FileName);
    if InModuleHandle<>0 then begin
        GetInModule:=GetInModuleFunc(GetProcAddress(InModuleHandle,'winampGetInModule2'));
    end;
    if @GetInModule<>nil then begin
        InputPlugin:=GetInModule;
    end;

    Result:=(InputPlugin<>nil);
end;

procedure FreeInModule;
begin
    if InModuleHandle<>0 then begin
        FreeLibrary(InModuleHandle);
        InModuleHandle:=0;
    end;
    InputPlugin:=nil;
end;

initialization

OutModuleHandle:=0;
OutputPlugin:=nil;
InModuleHandle:=0;
InputPlugin:=nil;

finalization

FreeOutModule;
FreeInModule;

end.
