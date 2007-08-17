program mp3TYRE;

{$APPTYPE CONSOLE}

uses
    Classes,Registry,StrUtils,SysUtils,Windows,
    FileUtils,MP3Struct,WinampPlugin;

type
    TMP3FileType=(mftNone,mftCBR,mftProCBR,mftVBR,mftProVBR);

    TMP3CheckResult=record
        FileType:TMP3FileType;
        HasID3v2Tag:Boolean;
        HasInvalidPadding:Boolean;
        HasIllegalPadding:Boolean;
        HasWAVEHeader:Boolean;
        HasJunkData:Boolean;
    end;

    TSegmentBuffer=array[0..65535] of Byte;
    PSegmentBuffer=^TSegmentBuffer;

var
    PluginSettings:TRegistry;
    WinampSettings:TRegistry;
    WinampPlugin:string;
    CleanFiles:Boolean;
    IndicateMP3Pro:Integer;
    MP3Extensions:TStringList;

const
    ReleaseNumber='10';
    CompileDate={$I %DATE%};
    mp3PROPlugIn='in_mp3PRO.dll';

function GetVersionString:string;
const
    Month:array[1..12] of string=(
        'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'
    );
begin
    Result:=ReleaseNumber
        +'.'+ExtractDelimited(1,CompileDate,['/'])
        +'.'+Month[StrToInt(ExtractDelimited(2,CompileDate,['/']))]
        +'.'+ExtractDelimited(3,CompileDate,['/']);
end;

function CheckMP3File(FileName:string):TMP3CheckResult;
const
    WAVE=$45564157;
    data=$61746164;
    MaxJunkBytes=32768;
var
    Input,Output:TFileStream;
    Tag1:TID3v1Tag;
    Tag2:TID3v2Tag;
    Buffer:PSegmentBuffer;
    BytesRead,PadBytes,JunkBytes:Integer;
    RIFF:TRIFFFile;
    Frame:TMP3Frame;
    Version:string;
begin
    Result.FileType:=mftNone;
    Result.HasID3v2Tag:=False;
    Result.HasInvalidPadding:=False;
    Result.HasIllegalPadding:=False;
    Result.HasWAVEHeader:=False;
    Result.HasJunkData:=False;

    Input:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
    if CleanFiles then begin
        Output:=TFileStream.Create(FileName+'.cleaned',fmCreate or fmShareExclusive);
    end;

    // Skip any ID3v2 tags.
    Tag2:=TID3v2Tag.Create;
    while Tag2.Read(Input) do begin
        WriteLn('Skipping ID3v',Tag2.GetVersionString,' tag of ',SizeOf(TID3v2Header)+Tag2.GetDataSize,' bytes.');
        if Tag2.HasInvalidPadding then begin
            WriteLn('The tag contains invalid (i.e. non-zero) padding.');
            Result.HasInvalidPadding:=True;
        end;
        if Result.HasID3v2Tag=False then begin
            // Write out only the first tag that has been found.
            if CleanFiles then begin
                // Be sure the tag is only written if CleanFiles is enabled.
                if not Tag2.Write(Output) then begin
                    WriteLn('Error writing tag to cleaned output file.');
                end;
            end;
            Result.HasID3v2Tag:=True;
        end;
    end;
    Tag2.Free;

    PadBytes:=0;
    if Result.HasID3v2Tag then begin
        // Some programs put illegal zero-bytes between the end of the tag and
        // the first MP3 frame. Allow up to 64kb of zero-bytes to be skipped.
        New(Buffer);
        BytesRead:=Input.Read(Buffer^,SizeOf(Buffer^));
        while (PadBytes<BytesRead) and (Buffer^[PadBytes]=0) do begin
            Inc(PadBytes);
        end;
        Input.Seek(-BytesRead+PadBytes,soFromCurrent);
        if PadBytes>0 then begin
            WriteLn('Skipping illegal padding of ',PadBytes,' bytes following the tag.');
            Result.HasIllegalPadding:=True;
        end;
        Dispose(Buffer);
    end;

    // Check for a WAVE header and move to the data chunk.
    RIFF:=TRIFFFile.Create(Input);
    if RIFF.IsValid and (RIFF.GetFormType=WAVE) then begin
        if RIFF.SearchChunk(data) then begin
            WriteLn('Skipping WAVE header (file size is ',RIFF.GetFileSize,' bytes).');
            Result.HasWAVEHeader:=True;
        end else begin
            WriteLn('Found broken WAVE header at offset ',Input.Position,'.');
        end;
    end;
    RIFF.Free;

    // Check if the file's first frame has a VBR header.
    // Limit the amout of junk data tolerated before the first frame.
    Frame:=TMP3Frame.Create;

    JunkBytes:=0;
    try
        repeat
            // Skip one byte if there no valid MP3 frame at this position.
            while (not Frame.Read(Input)) and (JunkBytes<MaxJunkBytes) do begin
                Input.Seek(1,soFromCurrent);
                Inc(JunkBytes);
            end;

            if Frame.IsValid then begin
                if Frame.IsVBR then begin
                    Result.FileType:=mftVBR;
                end else begin
                    Result.FileType:=mftCBR;
                end;

                Version:=Frame.GetVersionName;

                // Check two more frames to be sure this really is an MP3 file.
                if (not Frame.Read(Input)) or (not Frame.Read(Input)) then begin
                    Result.FileType:=mftNone;
                end;
            end;
        until (Result.FileType<>mftNone) or (JunkBytes=MaxJunkBytes);

        if Result.FileType<>mftNone then begin
            if JunkBytes>0 then begin
                WriteLn('Skipping junk data of ',JunkBytes,' bytes before first frame.');
                Result.HasJunkData:=True;
            end;
            WriteLn('Found ',Version,' audio frame.');
        end;
    except
        on ERangeError do begin
            WriteLn('Unexpected end of file.');
        end;
    end;

    if CleanFiles then begin
        // Copy all valid consecutive MP3 frames.
        Input.Seek(Output.Position+PadBytes+JunkBytes,soFromBeginning);
        JunkBytes:=0;
        repeat
            // Skip one byte if there no valid MP3 frame at this position.
            while (not Frame.Read(Input)) and (JunkBytes<MaxJunkBytes) do begin
                Input.Seek(1,soFromCurrent);
                Inc(JunkBytes);
            end;

            // We do not care for errors here.
            if JunkBytes<MaxJunkBytes then begin
                // Be sure the tag is only written if we did not reach MaxJunkBytes.
                if not Frame.Write(Output) then begin
                    WriteLn('Error writing frame to cleaned output file.');
                end;
            end;
        until JunkBytes=MaxJunkBytes;

        // If there is more data, and this data is an ID3v1 tag, copy it, too.
        Input.Seek(-ID3v1TagSize,soFromEnd);
        Tag1:=TID3v1Tag.Create;
        Tag1.Read(Input);
        Tag1.Write(Output);
        Tag1.Free;
    end;

    Frame.Free;

    Input.Free;
    if CleanFiles then begin
        Output.Free;
        DeleteFile(PChar(FileName));
        RenameFile(FileName+'.cleaned',FileName);
    end;
end;

{$HINTS OFF}
procedure SetInfoDummy(BitRate,SampleRate,Stereo:Integer;Synchronized:LongBool); cdecl;
begin
end;
{$HINTS ON}

function LoadInputPlugin(FileName:string):Boolean;
begin
    Write('Loading "'+FileName+'" ... ');
    Result:=LoadInModule(FileName);
    if Result then begin
        with InputPlugin^ do begin
            hMainWindow:=0;
            hDLLInstance:=InModuleHandle;

            SAAdd:=nil;
            VSAAdd:=nil;

            DSPIsActive:=nil;
            DSPDoSamples:=nil;

            SetInfo:=@SetInfoDummy;
            CurrentOutputPlugin:=nil;

            Init;
        end;
        WriteLn('found.');
        WriteLn('Input plug-in "',InputPlugin^.Description,'" successfully loaded.');
    end else begin
        WriteLn('not found.');
    end;
end;

procedure FreeInputPlugin;
begin
    if InputPlugin<>nil then begin
        InputPlugin^.Quit;
    end;
    FreeInModule;
end;

function IsPROFile(FileName:string):Boolean;
const
    mp3PRO=' - mp3PRO';
var
    Title:array[0..2047] of Char;
    LengthInMS:Integer;
begin
    Title[0]:=#0;
    LengthInMS:=0;
    if InputPlugin<>nil then begin
        // Playback will crash for invalid (very small) MP3 files.
        InputPlugin^.Play(PChar(FileName));
        InputPlugin^.GetFileInfo(nil,Title,LengthInMS);
        InputPlugin^.Stop;
    end;
    Result:=(StrComp(StrEnd(Title)-StrLen(mp3PRO),mp3PRO)=0) and (LengthInMS>0);
end;

function IdentifyMP3File(FileName:string):TMP3CheckResult;
begin
    Result:=CheckMP3File(FileName);
    if (Result.FileType<>mftNone) and IsPROFile(FileName) then begin
        Inc(Result.FileType);
    end;
end;

function ShortenString(Prefix,Text:string;MaxLength:Integer):string;
var
    Count:Integer;
begin
    Count:=Length(Text)-MaxLength;
    if Count>0 then begin
        Delete(Text,1,Count+Length(Prefix));
        Text:=Prefix+Text;
    end;
    Result:=Text;
end;

function AddTypeExtension(FileName:string;FileType:TMP3FileType):Boolean;
var
    Suffix,BaseName:string;
    i,l:Integer;
begin
    Suffix:=ExtractFileExt(FileName);
    BaseName:=Copy(FileName,1,Length(FileName)-Length(Suffix));

    i:=0;
    while i<MP3Extensions.Count do begin
        l:=Length(BaseName)-Length(MP3Extensions[i]);
        if LowerCase(Copy(BaseName,l+1,Length(MP3Extensions[i])))=LowerCase(MP3Extensions[i]) then begin
            BaseName:=Copy(BaseName,1,l);
            i:=0;
        end else begin
            Inc(i);
        end;
    end;

    Result:=RenameFile(FileName,BaseName+MP3Extensions[Ord(FileType)-1]+Suffix);
end;

procedure ProcessParameters;
var
    FullPath,RecurseDirectory,SimulateRename:Boolean;
    ProcessedTypes:set of TMP3FileType;

    AnalyzeCount:array[TMP3FileType] of Integer;
    TagCount,InvalidPadCount,IllegalPadCount,WAVECount,JunkCount:array[1..4] of Integer;
    ProcessCount,RenameCount:array[1..4] of Integer;

    procedure RenameFilesByType(Path:string);
    const
        BitrateMode:array[TMP3FileType] of string=(
            'no MP3',
            'a constant bitrate MP3','a constant bitrate mp3PRO',
            'a variable bitrate MP3','a variable bitrate mp3PRO'
        );
    var
        FileSearch:TFileSearch;
        i:Integer;
        CheckResult:TMP3CheckResult;
    begin
        if DirectoryExists(Path) then begin
            Path:=IncludeTrailingPathDelimiter(Path)+'*.mp3';
        end;

        FileSearch:=TFileSearch.Create;
        FileSearch.Recurse:=RecurseDirectory;
        FileSearch.Search(Path);
        for i:=0 to FileSearch.List.Count-1 do begin
            Path:=ExpandFileName(FileSearch.List[i]);

            Write('Analyzing file "');
            if FullPath then begin
                Write(Path);
            end else begin
                Write(ShortenString('[...]',Path,80-16-2-1));
            end;
            WriteLn('".');

            CheckResult:=IdentifyMP3File(Path);
            Inc(AnalyzeCount[CheckResult.FileType]);
            Inc(TagCount[Ord(CheckResult.FileType)],Ord(CheckResult.HasID3v2Tag));
            Inc(InvalidPadCount[Ord(CheckResult.FileType)],Ord(CheckResult.HasInvalidPadding));
            Inc(IllegalPadCount[Ord(CheckResult.FileType)],Ord(CheckResult.HasIllegalPadding));
            Inc(WAVECount[Ord(CheckResult.FileType)],Ord(CheckResult.HasWAVEHeader));
            Inc(JunkCount[Ord(CheckResult.FileType)],Ord(CheckResult.HasJunkData));

            Write('This is ',BitrateMode[CheckResult.FileType],' file');
            if CheckResult.FileType in ProcessedTypes then begin
                WriteLn('.');
                Inc(ProcessCount[Ord(CheckResult.FileType)]);
                if not SimulateRename then begin
                    if AddTypeExtension(Path,CheckResult.FileType) then begin
                        Inc(RenameCount[Ord(CheckResult.FileType)]);
                        Write('The file has successfully');
                    end else begin
                        Write('The file has not');
                    end;
                    WriteLn(' been renamed.');
                end else begin
                    WriteLn('The file has not been renamed (simulation mode).');
                end;
            end else begin
                WriteLn(', skipping.');
            end;
            
            WriteLn;
        end;
        FileSearch.Free;
    end;

    procedure WriteStatistics(Header:string;Data:array of Integer);
    begin
        Write(Header,Data[0]+Data[1]+Data[2]+Data[3]:6);
        WriteLn(' (',Data[0]:6,', ',Data[2]:6,', ',Data[1]:6,', ',Data[3]:6,')');
    end;

var
    i:Integer;
    Parameter:string;
begin
    // Set default values of available command line switches.
    CleanFiles:=False;
    FullPath:=False;
    RecurseDirectory:=False;
    SimulateRename:=True;
    ProcessedTypes:=[mftCBR,mftProCBR,mftVBR,mftProVBR];

    // Initialize statistic counters.
    FillChar(AnalyzeCount,SizeOf(AnalyzeCount),0);
    FillChar(TagCount,SizeOf(TagCount),0);
    FillChar(InvalidPadCount,SizeOf(InvalidPadCount),0);
    FillChar(IllegalPadCount,SizeOf(IllegalPadCount),0);
    FillChar(WAVECount,SizeOf(WAVECount),0);
    FillChar(JunkCount,SizeOf(JunkCount),0);

    FillChar(ProcessCount,SizeOf(ProcessCount),0);
    FillChar(RenameCount,SizeOf(RenameCount),0);

    for i:=1 to ParamCount do begin
        Parameter:=ParamStr(i);
        // Process command line switches.
        if Parameter[1]='/' then begin
            if Length(Parameter)=3 then begin
                // Toggle clean flag.
                if Parameter[2]='c' then begin
                    if Parameter[3]='+' then begin
                        CleanFiles:=True;
                    end else if Parameter[3]='-' then begin
                        CleanFiles:=False;
                    end;
                end;
                // Toggle path flag.
                if Parameter[2]='p' then begin
                    if Parameter[3]='+' then begin
                        FullPath:=True;
                    end else if Parameter[3]='-' then begin
                        FullPath:=False;
                    end;
                end;
                // Toggle recurse flag.
                if Parameter[2]='r' then begin
                    if Parameter[3]='+' then begin
                        RecurseDirectory:=True;
                    end else if Parameter[3]='-' then begin
                        RecurseDirectory:=False;
                    end;
                end;
                // Toggle simulate flag.
                if Parameter[2]='s' then begin
                    if Parameter[3]='+' then begin
                        SimulateRename:=True;
                    end else if Parameter[3]='-' then begin
                        SimulateRename:=False;
                    end;
                end;
            end else begin
                // Toggle processing of CBR files.
                if Copy(Parameter,2,3)='cbr' then begin
                    if Parameter[5]='+' then begin
                        Include(ProcessedTypes,mftCBR);
                    end else if Parameter[5]='-' then begin
                        Exclude(ProcessedTypes,mftCBR);
                    end;
                end;
                // Toggle processing of VBR files.
                if Copy(Parameter,2,3)='vbr' then begin
                    if Parameter[5]='+' then begin
                        Include(ProcessedTypes,mftVBR);
                    end else if Parameter[5]='-' then begin
                        Exclude(ProcessedTypes,mftVBR);
                    end;
                end;
                // Toggle processing of CBR mp3PRO files.
                if Copy(Parameter,2,6)='procbr' then begin
                    if Parameter[8]='+' then begin
                        Include(ProcessedTypes,mftProCBR);
                    end else if Parameter[8]='-' then begin
                        Exclude(ProcessedTypes,mftProCBR);
                    end;
                end;
                // Toggle processing of VBR mp3PRO files.
                if Copy(Parameter,2,6)='provbr' then begin
                    if Parameter[8]='+' then begin
                        Include(ProcessedTypes,mftProVBR);
                    end else if Parameter[8]='-' then begin
                        Exclude(ProcessedTypes,mftProVBR);
                    end;
                end;
            end;
        end else begin
            RenameFilesByType(Parameter);
        end;
    end;

    WriteLn('Statistics                Total     CBR     VBR  ProCBR  ProVBR  unknown');
    WriteLn('-------------------------------------------------------------------------');

    Write('Analyzed files       : ',AnalyzeCount[mftNone]+AnalyzeCount[mftCBR]+AnalyzeCount[mftVBR]+AnalyzeCount[mftProCBR]+AnalyzeCount[mftProVBR]:6);
    WriteLn(' (',AnalyzeCount[mftCBR]:6,', ',AnalyzeCount[mftVBR]:6,', ',AnalyzeCount[mftProCBR]:6,', ',AnalyzeCount[mftProVBR]:6,', ',AnalyzeCount[mftNone]:7,')');
    WriteStatistics(#195+' ID3v2 tag          : ',TagCount);
    WriteStatistics(#195+' Invalid padding    : ',InvalidPadCount);
    WriteStatistics(#195+' Illegal padding    : ',IllegalPadCount);
    WriteStatistics(#195+' WAVE header        : ',WAVECount);
    WriteStatistics(#192+' Junk data          : ',JunkCount);

    WriteStatistics('Processed files      : ',ProcessCount);
    WriteStatistics('Renamed successfully : ',RenameCount);
end;

begin
    WriteLn('mp3TYRE (MP3 TYpe REnamer) version ',GetVersionString);
    WriteLn('(C)opyright 2002-2007 by S. Schuberth <sschuberth_AT_gmail_DOT_com>',#13,#10);

    // In order to detect mp3PRO files, Coding Technologies' Winamp plug-in
    // for mp3PRO playback is required. You may download it at:
    // <http://www.mp3prozone.com/assets/mp3PROAudioDecoder.exe>

    // Tell the plug-in to indicate mp3PRO streams in Winamp's display.
    PluginSettings:=TRegistry.Create;
    PluginSettings.LazyWrite:=False;
    PluginSettings.RootKey:=HKEY_CURRENT_USER;

    // Default is to indicate mp3PRO files.
    IndicateMP3Pro:=1;
    if PluginSettings.OpenKey('\Software\Coding Technologies\mp3PROplugin for Winamp\Settings',False) then begin
        IndicateMP3Pro:=PluginSettings.ReadInteger('Indicate mp3PRO');
        PluginSettings.WriteInteger('Indicate mp3PRO',1);
    end;

    WriteLn('Searching for the mp3PRO Winamp plug-in.');
    if not LoadInputPlugin(mp3PROPlugIn) then begin
        WinampSettings:=TRegistry.Create;
        WinampSettings.RootKey:=HKEY_CURRENT_USER;

        if WinampSettings.OpenKey('\Software\Winamp',False) then begin
            WinampPlugin:=WinampSettings.ReadString('');
            WinampSettings.CloseKey;
        end;

        WinampSettings.Free;

        if (Length(WinampPlugin)=0) or not LoadInputPlugin(WinampPlugin+'\Plugins\'+mp3PROPlugIn) then begin
            WriteLn('mp3PRO files will not be detected.');
        end;
    end;

    WriteLn;

    if ParamCount>0 then begin
        MP3Extensions:=TStringList.Create;
        MP3Extensions.Add('.cbr');
        MP3Extensions.Add('.procbr');
        MP3Extensions.Add('.vbr');
        MP3Extensions.Add('.provbr');
        ProcessParameters;
        MP3Extensions.Free;
    end else begin
        WriteLn('Usage: mp3TYRE [switch]|<file|directory>|[switch]|[file|directory]|...');
        WriteLn;
        WriteLn('Switches (defaults are UPPER CASE):');
        WriteLn('/c<+|->',#9,#9,'Enable / DISABLE cleaning of MP3 files');
        WriteLn('/p<+|->',#9,#9,'Enable / DISABLE printing of full paths');
        WriteLn('/r<+|->',#9,#9,'Enable / DISABLE recursive processing of directories');
        WriteLn('/s<+|->',#9,#9,'ENABLE / disable simulation mode (files are not renamed)');
        WriteLn('/cbr<+|->',#9,'INCLUDE / exclude processing of constant bitrate MP3 files');
        WriteLn('/vbr<+|->',#9,'INCLUDE / exclude processing of variable bitrate MP3 files');
        WriteLn('/procbr<+|->',#9,'INCLUDE / exclude processing of CBR mp3PRO files');
        WriteLn('/provbr<+|->',#9,'INCLUDE / exclude processing of VBR mp3PRO files');
        WriteLn;
        WriteLn('Switches, files and directories may be specified in any order. Switches are');
        WriteLn('valid for all following files or directories until another switch is specified.');
    end;

    FreeInputPlugin;

    // Restore the original plug-in settings.
    if PluginSettings.CurrentKey<>0 then begin
        PluginSettings.WriteInteger('Indicate mp3PRO',IndicateMP3Pro);
        PluginSettings.CloseKey;
    end;
    PluginSettings.Free;
end.
