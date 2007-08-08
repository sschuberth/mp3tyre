unit MP3Struct;

interface

uses
    Classes;

const
    ID3v1TagSize=128;

    CHANNEL_MODE_STEREO      = $00;
    CHANNEL_MODE_JOINTSTEREO = $01;
    CHANNEL_MODE_DUALMONO    = $02;
    CHANNEL_MODE_SINGLEMONO  = $03;

    FRAME_EMPHASIS_NONE      = $00;
    FRAME_EMPHASIS_50_15MS   = $01;
    FRAME_EMPHASIS_CCITT_J17 = $03;

type
    // See <http://www.id3.org/id3v1.html> for details.
    TID3v1Tag=packed class(TObject)
      public
        function Read(MP3:TFileStream):Boolean;
        function Write(MP3:TFileStream):Boolean;
        function IsValid:Boolean;

      private
        Header:array[0..2] of Char;
        Title:array[0..29] of Char;
        Artist:array[0..29] of Char;
        Album:array[0..29] of Char;
        Year:array[0..3] of Char;
        Comment:array[0..29] of Char;
        Genre:Byte;
    end;

    TSynchsafeLongword=packed class(TObject)
      public
        function GetValue:Longword;
        procedure SetValue(Value:Longword);

      private
        Data:array[0..3] of Byte;

        function GetByte(Index:Integer):Byte;
        procedure SetByte(Index:Integer;Value:Byte);

      public
        property AccessByte[Index:Integer]:Byte read GetByte write SetByte; default;
    end;

    // See <http://www.id3.org/id3v2.4.0-structure.txt> for details.
    TID3v2Header=packed record
        Identifier:array[0..2] of Char;
        Version:Word;
        Flags:Byte;
        Size:TSynchsafeLongword;
    end;

    TID3v2Tag=packed class(TObject)
      public
        constructor Create;
        destructor Destroy; override;

        function Read(MP3:TFileStream):Boolean;
        function Write(MP3:TFileStream):Boolean;
        function IsValid:Boolean;

        function GetVersionString:string;

        function IsUnsynchronized:Boolean;
        function IsExtensionPresent:Boolean;
        function IsExperimental:Boolean;
        function IsFooterPresent:Boolean;

        function GetDataSize:Longword;

      private
        Header:TID3v2Header;
        Data:array of Byte;
    end;

    // "Resource Interchange File Format" structures.
    TRIFFChunk=packed record
        Identifier:array[0..3] of Char;
        Size:Longword;
    end;

    TRIFFHeader=packed record
        Form:TRIFFChunk;
        FormType:array[0..3] of Char;
    end;

    TRIFFFile=packed class(TObject)
      public
        constructor Create(RIFFStream:TFileStream);

        function IsValid:Boolean;
        function GetFileSize:Longword;
        function GetFormType:Longword;

        function GetChunkID:Longword;
        function GetChunkSize:Longword;

        function SkipChunk:Boolean;
        function SearchChunk(ChunkID:Longword):Boolean;

      private
        Stream:TFileStream;
        Header:TRIFFHeader;
        Chunk:TRIFFChunk;
    end;

    // MPEG 1.0 Layer III frame format.
    TMP3FrameHeader=Longword;

    TMP3Frame=packed class(TObject)
      public
        destructor Destroy; override;

        function Read(MP3:TFileStream):Boolean;
        function Write(MP3:TFileStream):Boolean;

        function GetSync:Word;
        function IsValid:Boolean;
        function IsVBR:Boolean;

        function GetVersion:Byte;
        function GetVersionName:String;
        function GetLayer:Byte;
        function GetLayerName:String;

        function IsProtected:Boolean;
        function GetBitRate:Integer;
        function GetSampleRate:Integer;
        function IsPadded:Boolean;
        function IsPrivate:Boolean;

        function GetChannelMode:Byte;
        function GetChannelModeName:String;
        function GetChannelModeExtension:Byte;

        function IsCopyrighted:Boolean;
        function IsOriginal:Boolean;

        function GetEmphasis:Byte;
        function GetEmphasisName:String;

        function GetDataSize:Integer;

      private
        Header:TMP3FrameHeader;
        Data:array of Byte;
    end;

implementation

type
    PIdentifier=^Longword;

// TSynchsafeLongword

function TSynchsafeLongword.GetByte(Index:Integer):Byte;
begin
    Result:=Data[Index];
end;

procedure TSynchsafeLongword.SetByte(Index:Integer;Value:Byte);
begin
    Data[Index]:=Value;
end;

function TSynchsafeLongword.GetValue:Longword;
begin
    Result:=GetByte(3)
        +(Longword(GetByte(2)) shl 7)
        +(Longword(GetByte(1)) shl 14)
        +(Longword(GetByte(0)) shl 21);
end;

procedure TSynchsafeLongword.SetValue(Value:Longword);
begin
    SetByte(3,Value and $7f);
    SetByte(2,(Value shr 7) and $7f);
    SetByte(1,(Value shr 14) and $7f);
    SetByte(0,(Value shr 21) and $7f);
end;

// TID3v1Tag

function TID3v1Tag.Read(MP3:TFileStream):Boolean;
var
    BytesRead:Integer;
begin
    Result:=False;

    BytesRead:=MP3.Read(Header,ID3v1TagSize);
    if (BytesRead<>ID3v1TagSize) or (not IsValid) then begin
        MP3.Seek(-BytesRead,soFromCurrent);
        Exit;
    end;

    Result:=True;
end;

function TID3v1Tag.Write(MP3:TFileStream):Boolean;
var
    BytesWritten:Integer;
begin
    Result:=False;

    // Do not write out an invalid tag.
    if not IsValid then
        Exit;

    BytesWritten:=MP3.Write(Header,ID3v1TagSize);
    if BytesWritten<>ID3v1TagSize then
        Exit;

    Result:=True;
end;

function TID3v1Tag.IsValid:Boolean;
begin
    Result:=(Header[0]='T') and (Header[1]='A') and (Header[2]='G');
end;

// TID3v2Tag

constructor TID3v2Tag.Create;
begin
    inherited Create;
    Header.Size:=TSynchsafeLongword.Create;
end;

destructor TID3v2Tag.Destroy;
begin
    Header.Size.Free;
    Finalize(Data);
    inherited Destroy;
end;

function TID3v2Tag.Read(MP3:TFileStream):Boolean;
var
    BytesRead:Integer;
begin
    Result:=False;

    // Using two calls is necessary to fill the "TID3v2Header" record because
    // the address of "TID3v2Header.Size" is not equal to the address of
    // "TID3v2Header.Size.Data" (as it would be for POD). This could be
    // avoided by using an "object" instead of a "class", but objects do not
    // support default array properties like "TSynchsafeLongword.AccessByte".
    BytesRead:=MP3.Read(Header,SizeOf(TID3v2Header)-SizeOf(Header.Size));
    if BytesRead<>SizeOf(TID3v2Header)-SizeOf(Header.Size) then begin
        MP3.Seek(-BytesRead,soFromCurrent);
        Exit;
    end;
    BytesRead:=MP3.Read(Header.Size.Data,SizeOf(Header.Size.Data));
    if BytesRead<>SizeOf(Header.Size.Data) then begin
        MP3.Seek(-BytesRead,soFromCurrent);
        Exit;
    end;

    if IsValid then begin
        SetLength(Data,GetDataSize);
        // Read in the frame data (do NOT read just to "Data" for dynamic arrays);
        BytesRead:=MP3.Read(Data[0],GetDataSize);
        if Longword(BytesRead)<>GetDataSize then begin
            MP3.Seek(-BytesRead,soFromCurrent);
            Finalize(Data);
        end else
            Result:=True;
    end else
        MP3.Seek(-SizeOf(TID3v2Header),soFromCurrent);
end;

function TID3v2Tag.Write(MP3:TFileStream):Boolean;
var
    BytesWritten:Integer;
begin
    Result:=False;

    // Do not write out an invalid tag.
    if not isValid then
        Exit;

    // Write first POD (Plain Old Data) part of the tag header.
    BytesWritten:=MP3.Write(Header,SizeOf(TID3v2Header)-SizeOf(Header.Size));
    if BytesWritten<>SizeOf(TID3v2Header)-SizeOf(Header.Size) then
        Exit;

    // Write the second part of the tag header (the field describing its size).
    BytesWritten:=MP3.Write(Header.Size.Data,SizeOf(Header.Size.Data));
    if BytesWritten<>SizeOf(Header.Size.Data) then
        Exit;

    // Write the tag data.
    BytesWritten:=MP3.Write(Data[0],GetDataSize);
    if Longword(BytesWritten)<>GetDataSize then
        Exit;

    Result:=True;
end;

function TID3v2Tag.IsValid:Boolean;
const
    ID3=$00334449;
begin
    Result:=(PIdentifier(@Header.Identifier)^ and $00ffffff=ID3)
        and (Lo(Header.Version)<$ff) and (Hi(Header.Version)<$ff)
        and (Header.Size[0]<$80) and (Header.Size[1]<$80)
        and (Header.Size[2]<$80) and (Header.Size[3]<$80);
end;

function TID3v2Tag.GetVersionString:string;
var
    MajorVersion,RevisionNumber:Char;
begin
    MajorVersion:=Chr(Ord('0')+Lo(Header.Version));
    RevisionNumber:=Chr(Ord('0')+Hi(Header.Version));
    Result:='2.'+MajorVersion+'.'+RevisionNumber;
end;

function TID3v2Tag.IsUnsynchronized:Boolean;
begin
    Result:=(Header.Flags and $80)<>0;
end;

function TID3v2Tag.IsExtensionPresent:Boolean;
begin
    Result:=(Header.Flags and $40)<>0;
end;

function TID3v2Tag.IsExperimental:Boolean;
begin
    Result:=(Header.Flags and $20)<>0;
end;

function TID3v2Tag.IsFooterPresent:Boolean;
begin
    Result:=(Header.Flags and $10)<>0;
end;

function TID3v2Tag.GetDataSize:Longword;
begin
    Result:=Header.Size.GetValue+Longword(IsFooterPresent)*10;
end;

// TRIFFFile

constructor TRIFFFile.Create(RIFFStream:TFileStream);
var
    BytesRead:Integer;
begin
    Stream:=RIFFStream;

    BytesRead:=Stream.Read(Header,SizeOf(TRIFFHeader));
    if (BytesRead<>SizeOf(TRIFFHeader)) or (not IsValid) then begin
        Stream.Seek(-BytesRead,soFromCurrent);
        Exit;
    end;

    BytesRead:=Stream.Read(Chunk,SizeOf(Chunk));
    if BytesRead<>SizeOf(Chunk) then
        Stream.Seek(-BytesRead,soFromCurrent);
end;

function TRIFFFile.IsValid:Boolean;
const
    RIFF=$46464952;
begin
    Result:=(PIdentifier(@Header.Form.Identifier)^=RIFF);
end;

function TRIFFFile.GetFileSize:Longword;
begin
    Result:=SizeOf(TRIFFChunk)+Header.Form.Size;
end;

function TRIFFFile.GetFormType:Longword;
begin
    Result:=PIdentifier(@Header.FormType)^;
end;

function TRIFFFile.GetChunkID:Longword;
begin
    Result:=PIdentifier(@Chunk.Identifier)^;
end;

function TRIFFFile.GetChunkSize:Longword;
begin
    Result:=Chunk.Size;
end;

function TRIFFFile.SkipChunk:Boolean;
var
    Position:Int64;
begin
    Result:=True;
    Position:=Stream.Position;
    try
        Stream.Seek((GetChunkSize+1) and (not 1),soFromCurrent);
        Position:=Stream.Position;
        Stream.ReadBuffer(Chunk,SizeOf(Chunk));
    except
        // on ERangeError,EReadError do
        Stream.Seek(Position,soFromBeginning);
        Result:=False;
    end;
end;

function TRIFFFile.SearchChunk(ChunkID:Longword):Boolean;
begin
    Result:=True;
    while (GetChunkID<>ChunkID) and Result do
        Result:=SkipChunk;
end;

// TMP3Frame

destructor TMP3Frame.Destroy;
begin
    Finalize(Data);
    inherited Destroy;
end;

function TMP3Frame.Read(MP3:TFileStream):Boolean;
var
    BytesRead:Integer;
begin
    Result:=False;

    BytesRead:=MP3.Read(Header,SizeOf(TMP3FrameHeader));
    if BytesRead<>SizeOf(TMP3FrameHeader) then begin
        MP3.Seek(-BytesRead,soFromCurrent);
        Exit;
    end;

    // Convert big endian to little endian byte order.
    Header:=(Swap(Word(Header)) shl 16) or Swap(Word(Header shr 16));

    if IsValid then begin
        SetLength(Data,GetDataSize);
        // Read in the frame data (do NOT read just to "Data" for dynamic arrays).
        BytesRead:=MP3.Read(Data[0],GetDataSize);
        if BytesRead<>GetDataSize then begin
            MP3.Seek(-BytesRead,soFromCurrent);
            Finalize(Data);
        end else
            Result:=True;
    end else
        MP3.Seek(-SizeOf(TMP3FrameHeader),soFromCurrent);
end;

function TMP3Frame.Write(MP3:TFileStream):Boolean;
var
    BytesWritten,DataSize:Integer;
begin
    Result:=False;

    // Do not write out an invalid frame.
    if not isValid then
        Exit;

    // Convert little endian back to big endian byte order.
    DataSize:=GetDataSize;
    Header:=(Swap(Word(Header)) shl 16) or Swap(Word(Header shr 16));

    // Write the frame header.
    BytesWritten:=MP3.Write(Header,SizeOf(TMP3FrameHeader));
    if BytesWritten<>SizeOf(TMP3FrameHeader) then
        Exit;

    // Write the frame data.
    BytesWritten:=MP3.Write(Data[0],DataSize);
    if BytesWritten<>DataSize then
        Exit;

    Result:=True;
end;

function TMP3Frame.IsValid:Boolean;
begin
    Result:=(GetSync=$07ff) and (GetVersion>0) and (GetLayer>0)
        and (GetBitRate>-1) and (GetSampleRate>-1) and (GetEmphasis<>2)
        and (GetDataSize>0);
end;

function TMP3Frame.IsVBR:Boolean;
const
    Xing=$676e6958;
    VBRI=$49524256;
var
    Offset:Longword;
    VBRHeader:PIdentifier;
begin
    // For details refer to the Xing VBR header SDK which is still available at:
    // <http://docs.real.com/docs/xingtech/vbrheadersdk.zip>
    if GetVersion=1 then begin
        if GetChannelMode<>CHANNEL_MODE_SINGLEMONO then
            Offset:=32
        else
            Offset:=17;
    end else begin
        if GetChannelMode<>CHANNEL_MODE_SINGLEMONO then
            Offset:=17
        else
            Offset:=9;
    end;
    VBRHeader:=@Data[Offset];
    Result:=(VBRHeader^=Xing);

    // See <http://minnie.tuhs.org/pipermail/mp3encoder/2001-January/001800.html>
    // for details.
    if not Result then begin
        VBRHeader:=@Data[32];
        Result:=(VBRHeader^=VBRI);
    end;
end;

function TMP3Frame.GetSync:Word;
begin
    Result:=(Header and $ffe00000) shr 21;
end;

function TMP3Frame.GetVersion:Byte;
const
    Version:array[0..3] of Byte=(3,0,2,1);
begin
    // Returns 1 for MPEG 1.0, 2 for MPEG 2.0 and 3 for MPEG 2.5.
    Result:=Version[(Header and $00180000) shr 19];
end;

function TMP3Frame.GetVersionName:String;
const
    Name:array[0..3] of string=('<invalid>','MPEG 1.0','MPEG 2.0','MPEG 2.5');
begin
    Result:=Name[GetVersion];
end;

function TMP3Frame.GetLayer:Byte;
begin
    Result:=(4-((Header and $00060000) shr 17)) and $03;
end;

function TMP3Frame.GetLayerName:String;
const
    Name:array[0..3] of string=('<invalid>','Layer I','Layer II','Layer III');
begin
    Result:=Name[GetLayer];
end;

function TMP3Frame.IsProtected:Boolean;
begin
    Result:=(Header and $00010000)=0;
end;

function TMP3Frame.GetBitRate:Integer;
const
    BitRate:array[1..3,1..3,0..15] of Integer=(
        ((   0,  32,  64,  96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448,  -1),  // MPEG 1.0, Layer I
         (   0,  32,  48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384,  -1),  // MPEG 1.0, Layer II
         (   0,  32,  40,  48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320,  -1)), // MPEG 1.0, Layer III
        ((   0,  32,  48,  56,  64,  80,  96, 112, 128, 144, 160, 176, 192, 224, 256,  -1),  // MPEG 2.0, Layer I
         (   0,   8,  16,  24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160,  -1),  // MPEG 2.0, Layer II
         (   0,   8,  16,  24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160,  -1)), // MPEG 2.0, Layer III
        ((   0,  32,  48,  56,  64,  80,  96, 112, 128, 144, 160, 176, 192, 224, 256,  -1),  // MPEG 2.5, Layer I
         (   0,   8,  16,  24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160,  -1),  // MPEG 2.5, Layer II
         (   0,   8,  16,  24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160,  -1))  // MPEG 2.5, Layer III
    );
begin
    Result:=BitRate[GetVersion,GetLayer,(Header and $0000f000) shr 12];
end;

function TMP3Frame.GetSampleRate:Integer;
const
    SampleRate:array[1..3,0..3] of Integer=(
        (44100,48000,32000,-1), // MPEG 1.0
        (22050,24000,16000,-1), // MPEG 2.0
        (11025,12000,8000,-1)   // MPEG 2.5
    );
begin
    Result:=SampleRate[GetVersion,(Header and $00000c00) shr 10];
end;

function TMP3Frame.IsPadded:Boolean;
begin
    Result:=(Header and $00000200)<>0;
end;

function TMP3Frame.IsPrivate:Boolean;
begin
    Result:=(Header and $00000100)<>0;
end;

function TMP3Frame.GetChannelMode:Byte;
begin
    Result:=(Header and $000000c0) shr 6;
end;

function TMP3Frame.GetChannelModeName:String;
const
    Name:array[0..3] of string=(
        'Stereo','Joint stereo','Dual mono','Single mono'
    );
begin
    Result:=Name[GetChannelMode];
end;

function TMP3Frame.GetChannelModeExtension:Byte;
begin
    Result:=(Header and $00000030) shr 4;
end;

function TMP3Frame.IsCopyrighted:Boolean;
begin
    Result:=(Header and $00000008)<>0;
end;

function TMP3Frame.IsOriginal:Boolean;
begin
    Result:=(Header and $00000004)<>0;
end;

function TMP3Frame.GetEmphasis:Byte;
begin
    Result:=Header and $00000003;
end;

function TMP3Frame.GetEmphasisName:String;
const
    Name:array[0..3] of string=('None','50 / 15 ms','<invalid>','CCITT J.17');
begin
    Result:=Name[GetEmphasis];
end;

function TMP3Frame.GetDataSize:Integer;
var
    Factor,BytesPerSlot:Integer;
begin
    // See <http://board.mp3-tech.org/view.php3?bn=agora_mp3techorg&key=999041716>
    // for details or ask Rob Leslie <rob_AT_mars_DOT_org>.
    if GetLayer=1 then begin
        Factor:=12;
        BytesPerSlot:=4;
    end else begin
        Factor:=144;
        BytesPerSlot:=1;
    end;

    if GetVersion>1 then
        Factor:=Factor div 2;
    Result:=((Factor*GetBitRate*1000 div GetSampleRate)+Ord(IsPadded))*BytesPerSlot;

    // Subtract the frame header size from the frame length.
    Dec(Result,SizeOf(TMP3FrameHeader));
end;

end.
