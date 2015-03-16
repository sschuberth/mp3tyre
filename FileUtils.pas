{
    File handling related utility implementations
    
    This is a wrapper class around FindFirst / FindNext to (recursively) search
    for files matching a given mask and / or attributes.
}

unit FileUtils;

interface

uses
    Classes;

type
    TFileAttributes=(faReadOnly,faHidden,faSysFile,faVolumeID,faDirectory,faArchive,faAnyFile);
    TAttributeSet=set of TFileAttributes;

    TFileSearch=class(TObject)
      public
        constructor Create;
        destructor Destroy; override;

        procedure Search(const Directory,Mask:string); overload;
        procedure Search(const Path:string); overload;

      private
        FRecurse:Boolean;
        FAttributes:Integer;
        FList:TStringList;

        function GetAttributes:TAttributeSet;
        procedure SetAttributes(Attributes:TAttributeSet);

        function GetRecurse:Boolean;
        procedure SetRecurse(Recurse:Boolean);

        function GetList:TStringList;

      published
        property Attributes:TAttributeSet read GetAttributes write SetAttributes;
        property Recurse:Boolean read GetRecurse write SetRecurse;
        property List:TStringList read GetList;
    end;

implementation

uses
    SysUtils;

constructor TFileSearch.Create;
begin
    inherited Create;
    Recurse:=True;
    Attributes:=[faReadOnly,faArchive];
    FList:=TStringList.Create;
end;

destructor TFileSearch.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;

function TFileSearch.GetAttributes:TAttributeSet;
begin
    Result:=[];
    if (FAttributes and SysUtils.faReadOnly)=SysUtils.faReadOnly then begin
        Include(Result,faReadOnly);
    end;
    if (FAttributes and SysUtils.faHidden)=SysUtils.faHidden then begin
        Include(Result,faHidden);
    end;
    if (FAttributes and SysUtils.faSysFile)=SysUtils.faSysFile then begin
        Include(Result,faSysFile);
    end;
    if (FAttributes and SysUtils.faVolumeID)=SysUtils.faVolumeID then begin
        Include(Result,faVolumeID);
    end;
    if (FAttributes and SysUtils.faDirectory)=SysUtils.faDirectory then begin
        Include(Result,faDirectory);
    end;
    if (FAttributes and SysUtils.faArchive)=SysUtils.faArchive then begin
        Include(Result,faArchive);
    end;
    if (FAttributes and SysUtils.faAnyFile)=SysUtils.faAnyFile then begin
        Include(Result,faAnyFile);
    end;
end;

procedure TFileSearch.SetAttributes(Attributes:TAttributeSet);
begin
    FAttributes:=0;
    if (faReadOnly in Attributes) then begin
        FAttributes:=FAttributes or SysUtils.faReadOnly;
    end;
    if (faHidden in Attributes) then begin
        FAttributes:=FAttributes or SysUtils.faHidden;
    end;
    if (faSysFile in Attributes) then begin
        FAttributes:=FAttributes or SysUtils.faSysFile;
    end;
    if (faVolumeID in Attributes) then begin
        FAttributes:=FAttributes or SysUtils.faVolumeID;
    end;
    if (faDirectory in Attributes) then begin
        FAttributes:=FAttributes or SysUtils.faDirectory;
    end;
    if (faArchive in Attributes) then begin
        FAttributes:=FAttributes or SysUtils.faArchive;
    end;
    if (faAnyFile in Attributes) then begin
        FAttributes:=FAttributes or SysUtils.faAnyFile;
    end;
end;

function TFileSearch.GetRecurse:Boolean;
begin
    Result:=FRecurse;
end;

procedure TFileSearch.SetRecurse(Recurse:Boolean);
begin
    FRecurse:=Recurse;
end;

function TFileSearch.GetList:TStringList;
begin
    Result:=FList;
end;

procedure TFileSearch.Search(const Directory,Mask:string);
var
    FileInfo:TSearchRec;
begin
    if FindFirst(Directory+Mask,FAttributes and (not SysUtils.faDirectory),FileInfo)=0 then begin
        try
            repeat
                FList.Add(Directory+FileInfo.Name);
            until FindNext(FileInfo)<>0;
        finally
            SysUtils.FindClose(FileInfo);
        end;
    end;

    if not FRecurse then begin
        Exit;
    end;

    if FindFirst(Directory+'*.*',SysUtils.faDirectory,FileInfo)=0 then begin
        try
            repeat
                if ((FileInfo.Attr and SysUtils.faDirectory)=SysUtils.faDirectory) and (FileInfo.Name[1]<>'.') then begin
                    Search(IncludeTrailingPathDelimiter(Directory+FileInfo.Name),Mask);
                end;
            until FindNext(FileInfo)<>0;
        finally
            SysUtils.FindClose(FileInfo);
        end;
    end;
end;

procedure TFileSearch.Search(const Path:string);
var
    Directory:string;
begin
    Directory:=IncludeTrailingPathDelimiter(ExtractFilePath(Path));
    if Directory='\' then begin
        Directory:='';
    end;
    Search(Directory,ExtractFileName(Path));
end;

end.
