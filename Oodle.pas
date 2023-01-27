unit Oodle;

{
  Oodle implementation

  TetzkatLipHoka - 2023
}

interface

{$DEFINE AUTO_INIT}               // AutoInit/DeInit Oodle(s) during initialization/finalization
{.$DEFINE VERCHECK}                // Perform Version-Check and inform on missmatch
{$DEFINE OnlyWarnOnLowerVersions} 
{.$DEFINE SILENT}                 // Don't show any warnings (missing Oodle, Version-Missmatch) but Linking Errors
{.$DEFINE CLOSE_APP_ON_FAIL}
{$DEFINE WARN_DLLs_IN_FOLDER}     // Warning if DLLs are present in Application-Folder
{.$DEFINE IGNORE_LINKING_ERRORS}  // Ignore linking errors (DEBUG)
{.$DEFINE LIST_MISSING_MODULES}   // uses MemoryModule

// ResourceFile
{$DEFINE ResourceMode}            // Load DLL from Resourcefile
{$IFDEF ResourceMode}
//  {$R Oodle.res}                    // DLL-ResourceFile (each DLLName without extension as RCDATA)
  {$IFDEF Win64}
  {$R oo2core_7_win64.res}        // DLL-ResourceFile (each DLLName without extension as RCDATA)
  {$ELSE}
  {$R oo2core_5_win32.res}        // DLL-ResourceFile (each DLLName without extension as RCDATA)
  {$ENDIF}
{$ENDIF}

{$DEFINE ResourceCompression}     // DLLs in ResourceFile are 7zip compressed (JEDI-Unit)
{$DEFINE PREFER_DLL_IN_FOLDER}    // If DLL is present in ApplicationFolder use it
{$DEFINE MemoryModule}            // use MemoryModule (Load without HDD-caching)

{$DEFINE GetModuleHandle}         // Try GetModuleHandle before LoadLibrary
{.$DEFINE ONLY_LOADLIBRARY_ERRORS} // Ignore 'GetLastError <> ERROR_SUCCESS' unless LoadLibrary actually failed
{.$DEFINE NOAUTO_DEINIT}

{.$DEFINE TESTCASE}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$DEFINE section_INTERFACE_USES}
{$I DynamicDLL.inc}
{$UNDEF section_INTERFACE_USES}
,types
;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$DEFINE section_INTERFACE}
{$I DynamicDLL.inc}
{$UNDEF section_INTERFACE}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Constantes~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
const
  DLLCount_   = 1;
var
  DLLPath_    : Array [0..DLLCount_-1] of String = ( '' );
const
  DLLLoadDLL_ : Array [0..DLLCount_-1] of boolean = ( True );
  {$IFDEF Win64}
  DLLName_    : Array [0..DLLCount_-1] of String = ( 'oo2core_7_win64.dll' );
  {$ELSE}
  DLLName_    : Array [0..DLLCount_-1] of String = ( 'oo2core_5_win32.dll' );
  {$ENDIF}
  DLLVersion_ : Array [0..DLLCount_-1] of String = ( '' );
  {$IFDEF ResourceMode}
  DLLPass_    : Array [0..DLLCount_-1] of String = ( '' );
  {$ENDIF}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Oodle Declarations~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$WARNINGS OFF}

{$DEFINE section_Declaration} // Include
{.$I Oodle.inc}
{$UNDEF section_Declaration}

type
  Toodle_NoAssert = function( const a : PAnsiChar; const b : Integer; const c : PAnsiChar; const d : PAnsiChar ) : Integer; stdcall;

  TOodleAlgo = (
    oaLZH         = 0,
    oaLZHLW       = 1,
    oaLZNIB       = 2,
    oaNone        = 3,
    oaLZB16       = 4,
    oaLZBLW       = 5,
    oaLZA         = 6,
    oaLZNA        = 7,
    oaKraken      = 8,
    oaMermaid     = 9,
    oaBitKnit     = 10,
    oaSelkie      = 11,
    oaAkkorokamui = 12,

    oaLZQ1        = 8, // old name of Kraken
    oaLZNIB2      = 9  // old name of Mermaid
  );

const
  OodleAlgoName : Array [ oaLZH..oaAkkorokamui ] of AnsiString = (
    'LZH',
    'LZHLW',
    'LZNIB',
    'None',         // 0x8c->0xcc
    'LZB16',
    'LZBLW',
    'LZA',
    'LZNA',
    'Kraken',
    'Mermaid',
    'BitKnit',
    'Selkie',
    'Akkorokamui'

//    'LZQ1',   // old name of Kraken
//    'LZNIB2' // old name of Mermaid
  );

  OodleAlgoRAW : Array [ oaLZH..oaAkkorokamui ] of Byte = (
    7,
    0,
    1,
    7,
    2,
    3,
    4,
    5,
    6,
    10,
    11,
    10,
    6
  );

var
  OodleLZ_Compress : function( Algo : Integer; Source : PByte; Size : Integer; Destination : PByte; DestSize : Integer; A : Pointer = nil; B : Pointer = nil; C : Pointer = nil ) : Integer; stdcall = NIL;
  OodleLZ_Decompress : function( Source : PByte; Size : Integer; Destination : PByte; DestSize : Integer; A : Integer; B : Integer; C : Integer; D : Pointer = nil; E : Pointer = nil; F : Pointer = nil; G : Pointer = nil; H : Pointer = nil; I : Pointer = nil; J : Integer = 0 ) : Integer; stdcall = NIL; // Oodle 2.3.0
  OodlePlugins_SetAssertion : procedure( func : Toodle_NoAssert ); stdcall = NIL;

function OodlePlugins_NoAssert : Integer; stdcall;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function CompressOodle( Source : PByte; Len : Cardinal; var Compressed : TByteDynArray; Algo : TOodleAlgo = oaBitKnit; Header : boolean = False ) : Int64; overload;
{$IFNDEF Win64}
function CompressOodle( Source : PByte; Len : Cardinal; var Compressed : Pointer; Algo : TOodleAlgo = oaBitKnit; Header : boolean = False ) : Int64; overload;
{$ENDIF}
function ExtractOodle( Source : PByte; Len : Cardinal; var Decompressed : TByteDynArray ) : Int64; overload;
{$IFNDEF Win64}
function ExtractOodle( Source : PByte; Len : Cardinal; var Decompressed : Pointer ) : Int64; overload;
{$ENDIF}

function CompressStreamOodle( Source : TStream; Compressed : TStream; Algo : TOodleAlgo = oaBitKnit; Header : boolean = False ) : Int64; overload;
function CompressStreamOodle( var Source : TStream; Algo : TOodleAlgo = oaBitKnit; Header : boolean = False ) : Int64; overload;

function ExtractStreamOodle( Source : TStream; DeCompressed : TStream ) : Int64; overload;
function ExtractStreamOodle( var Source : TStream ) : Int64; overload;

{$IFDEF TESTCASE}
function TestOodle( FileName : string; Header : boolean = False; SaveDebugFiles : Boolean = False ) : Int64;
{$ENDIF TESTCASE}

{$WARNINGS ON}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

implementation

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$DEFINE section_IMPLEMENTATION_USES}
{$I DynamicDLL.inc}
{$UNDEF section_IMPLEMENTATION_USES}

{$IFDEF TESTCASE}
  ,Dialogs
{$ENDIF TESTCASE}
;

{$DEFINE section_IMPLEMENTATION_INITVAR}
{$I DynamicDLL.inc}
{$UNDEF section_IMPLEMENTATION_INITVAR}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure PreInitialization;
begin
  // Code needed before Init, likely change Oodle OS-Dependent
end;

procedure InitDLL( ID : Byte; StrL : TStringList );
begin
  {$DEFINE section_InitVar}
  {$WARNINGS OFF}

  case ID of
    0 : begin
        {.$I Oodle.inc} // Include

        {$IFDEF Win64}
        InitVar( @OodleLZ_Compress, 'OodleLZ_Compress', ID, StrL );
        InitVar( @OodleLZ_Decompress, 'OodleLZ_Decompress', ID, StrL );
        InitVar( @OodlePlugins_SetAssertion, 'OodleCore_Plugins_SetAssertion', ID, StrL );
//        InitVar( @OodlePlugins_SetAssertion, 'OodlePlugins_SetAssertion', ID, StrL );
        {$ELSE}
        InitVar( @OodleLZ_Compress, '_OodleLZ_Compress@32', ID, StrL );
        InitVar( @OodleLZ_Decompress, '_OodleLZ_Decompress@56', ID, StrL );
        InitVar( @OodlePlugins_SetAssertion, '_OodlePlugins_SetAssertion@4', ID, StrL );
        {$ENDIF}
        end;
  end;

  {$WARNINGS ON}
  {$UNDEF section_InitVar}
end;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Redirects~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$DEFINE section_Redirects}
{.$I Oodle.inc}
{$UNDEF section_Redirects}

function OodlePlugins_NoAssert : Integer; stdcall;
begin
  result := 0;
end;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}

const
  HEADER_ = AnsiString( 'oodle' );
  HEADER_LEN_ = Length( HEADER_ );
type
  tHeader = Array [ 0..HEADER_LEN_-1 ] of AnsiChar;
  pHeader = ^tHeader;

function CompressOodle( Source : PByte; Len : Cardinal; var Compressed : TByteDynArray; Algo : TOodleAlgo = oaBitKnit; Header : boolean = False ) : Int64;
var
  Offset : Cardinal;
begin
  result := -102;
  if NOT Oodle.IsInitDLL then
    Exit;

  result := -101;
  if NOT Assigned( Source ) OR ( Len = 0 ) then
    Exit;
  SetLength( Compressed, 0 );

  result := -100;
  if ( Algo = oaNone ) OR ( ( Algo = oaKraken ) AND ( Len < 500*1024 ) ) then
    Exit;

  if Header then
    Offset := HEADER_LEN_+SizeOf( Len )
  else
    Offset := SizeOf( Len );
  SetLength( Compressed, Offset+Len );

  if Header then
    begin
    Move( HEADER_[ 1 ], Compressed[ 0 ], HEADER_LEN_ );
    Move( Len, Compressed[ HEADER_LEN_ ], SizeOf( Len ) );
    end
  else
    Move( Len, Compressed[ 0 ], SizeOf( Len ) );

  result := OodleLZ_Compress( Integer( Algo ), Source, Len, @Compressed[ Offset ], 7{Rate/Max}, nil, nil, nil );
  if ( result > 0 ) then
    begin
    Result := Offset+result;
    SetLength( Compressed, result );
    end
  else
    SetLength( Compressed, 0 );
end;

{$IFNDEF Win64}
function CompressOodle( Source : PByte; Len : Cardinal; var Compressed : Pointer; Algo : TOodleAlgo = oaBitKnit; Header : boolean = False ) : Int64;
var
  bCompressed : PByte;
  Offset : Cardinal;
begin
  result := -102;
  if NOT Oodle.IsInitDLL then
    Exit;

  result := -101;
  if NOT Assigned( Source ) OR ( Len = 0 ) then
    Exit;
  if Assigned( Compressed ) then
    ReallocMem( Compressed, 0 );

  result := -100;
  if ( Algo = oaNone ) OR ( ( Algo = oaKraken ) AND ( Len < 500*1024 ) ) then
    Exit;

  if Header then
    Offset := HEADER_LEN_+SizeOf( Len )
  else
    Offset := SizeOf( Len );
  ReallocMem( Compressed, Offset+Len );

  bCompressed := Compressed;
  if Header then
    begin
    Move( HEADER_[ 1 ], bCompressed^, HEADER_LEN_ );
    Inc( bCompressed, HEADER_LEN_ );
    end;
  Move( Len, bCompressed^, SizeOf( Len ) );
  Inc( bCompressed, SizeOf( Len ) );

  result := OodleLZ_Compress( Integer( Algo ), Source, Len, bCompressed, 7{Rate/Max}, nil, nil, nil );
  if ( result > 0 ) then
    begin
    Result := Offset+result;
    ReallocMem( Compressed, result );
    end
  else
    ReallocMem( Compressed, 0 );
end;
{$ENDIF}

function CompressStreamOodle( Source : TStream; Compressed : TStream; Algo : TOodleAlgo = oaBitKnit; Header : boolean = False ) : Int64;
var
  S : TMemoryStream;
  tmp : TByteDynArray;
begin
  if ( Source = Compressed ) then
    begin
    result := CompressStreamOodle( Source, Algo, Header );
    Exit;
    end;

  result := -4;
  if NOT Assigned( Source ) then
    Exit;

  result := -3;
  if ( Source.Size <= 0 ) then
    Exit;

  result := -2;
  if NOT Assigned( Compressed ) then
    Exit;

  S := TMemoryStream.Create;
  S.CopyFrom( Source, Source.Size-Source.Position );
  S.Position := 0;

  SetLength( tmp, S.Size );
  Result := CompressOodle( S.Memory, S.Size, tmp, Algo, Header );

  if ( result > 0 ) then
    begin
//    Compressed.Position := 0;
//    Compressed.Size := 0;
    Compressed.Write( tmp[ 0 ], result );
    end
  else
    result := -1;

  SetLength( tmp, 0 );
  S.Free;
end;

function CompressStreamOodle( var Source : TStream; Algo : TOodleAlgo = oaBitKnit; Header : boolean = False ) : Int64;
var
  S : TMemoryStream;
  tmp : TByteDynArray;
begin
  result := -3;
  if NOT Assigned( Source ) then
    Exit;

  result := -2;
  if ( Source.Size <= 0 ) then
    Exit;

  S := TMemoryStream.Create;
  S.CopyFrom( Source, Source.Size-Source.Position );
  S.Position := 0;

  SetLength( tmp, S.Size );
  Result := CompressOodle( S.Memory, S.Size, tmp, Algo, Header );

  if ( result > 0 ) then
    begin
    Source.Position := 0;
    Source.Size := 0;
    Source.Write( tmp[ 0 ], result );
    end
  else
    result := -1;

  SetLength( tmp, 0 );
  S.Free;
end;

function ExtractStreamOodle( Source : TStream; DeCompressed : TStream ) : Int64;
var
  S : TMemoryStream;
  tmp : TByteDynArray;
begin
  if ( Source = DeCompressed ) then
    begin
    result := ExtractStreamOodle( Source );
    Exit;
    end;

  result := -4;
  if NOT Assigned( Source ) then
    Exit;

  result := -3;
  if ( Source.Size <= 0 ) then
    Exit;

  result := -2;
  if NOT Assigned( DeCompressed ) then
    Exit;

  S := TMemoryStream.Create;
  S.CopyFrom( Source, Source.Size-Source.Position );
  S.Position := 0;

  SetLength( tmp, S.Size );
  Result := ExtractOodle( S.Memory, S.Size, tmp );

  if ( result > 0 ) then
    begin
//    Compressed.Position := 0;
//    Compressed.Size := 0;
    DeCompressed.Write( tmp[ 0 ], result );
    end
  else
    result := -1;

  SetLength( tmp, 0 );
  S.Free;
end;

function ExtractStreamOodle( var Source : TStream ) : Int64;
var
  S : TMemoryStream;
  tmp : TByteDynArray;
begin
  result := -3;
  if NOT Assigned( Source ) then
    Exit;

  result := -2;
  if ( Source.Size <= 0 ) then
    Exit;

  S := TMemoryStream.Create;
  S.CopyFrom( Source, Source.Size-Source.Position );
  S.Position := 0;

  SetLength( tmp, S.Size );
  Result := ExtractOodle( S.Memory, S.Size, tmp );

  if ( result > 0 ) then
    begin
    Source.Position := 0;
    Source.Size := 0;
    Source.Write( tmp[ 0 ], result );
    end
  else
    result := -1;

  SetLength( tmp, 0 );
  S.Free;
end;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function ExtractOodle( Source : PByte; Len : Cardinal; var Decompressed : TByteDynArray ) : Int64;
var
  OutSize : Cardinal;
begin
  result := -101;
  if NOT Oodle.IsInitDLL then
    Exit;

  result := -100;
  if NOT Assigned( Source ) OR ( Len = 0 ) then
    Exit;
  SetLength( Decompressed, 0 );

  if ( pHeader( Source )^ = HEADER_ ) then
    begin
    Inc( Source, HEADER_LEN_ );
    Dec( Len, HEADER_LEN_ );
    end;

  OutSize := PCardinal( Source )^;
  Inc( Source, SizeOf( OutSize ) );
  Dec( Len, SizeOf( OutSize ) );

//  OutSize := Len*4;
  SetLength( Decompressed, OutSize );

  result := OodleLZ_Decompress( Source, Len, @Decompressed[ 0 ], OutSize, 0, 0, 0, nil, nil, nil, nil, nil, nil, 3 );
  if ( result = OutSize ) then
    begin
//    result := OutSize;
    SetLength( Decompressed, result );
    end
  else
    SetLength( Decompressed, 0 );
end;

{$IFNDEF Win64}
function ExtractOodle( Source : PByte; Len : Cardinal; var Decompressed : Pointer ) : Int64;
var
  OutSize        : Cardinal;
begin
  result := -101;
  if NOT Oodle.IsInitDLL then
    Exit;

  result := -100;
  if NOT Assigned( Source ) OR ( Len = 0 ) then
    Exit;
  if Assigned( Decompressed ) then
    ReallocMem( Decompressed, 0 );

  if ( pHeader( Source )^ = HEADER_ ) then
    begin
    Inc( Source, HEADER_LEN_ );
    Dec( Len, HEADER_LEN_ );
    end;

  OutSize := PCardinal( Source )^;
  Inc( Source, SizeOf( OutSize ) );
  Dec( Len, SizeOf( OutSize ) );

//  OutSize := Len*4;
  GetMem( Decompressed, OutSize );

  result := OodleLZ_Decompress( Source, Len, Decompressed, OutSize, 0, 0, 0, nil, nil, nil, nil, nil, nil, 3 );
  if ( result = OutSize ) then
    begin
    result := OutSize;
    ReallocMem( Decompressed, result )
    end
  else
    ReallocMem( Decompressed, 0 );
end;
{$ENDIF Win64}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$IFDEF TESTCASE}
function TestOodle( FileName : string; Header : boolean = False; SaveDebugFiles : Boolean = False ) : Int64;
var
  sIn : TMemoryStream;

  function PointerToArray( Algo : TOodleAlgo ) : Int64;
  var
    sCompress, sOut : TMemoryStream;
    aDecompressed : TByteDynArray;
  begin
    SetLength( aDecompressed, 0 );

    sCompress := TMemoryStream.Create;
    result := CompressOodle( sIn.Memory, sIn.Size, aDecompressed, Algo, Header );
    if ( result <= 0 ) then
      begin
      sCompress.free;
      result := -3;
      Exit;
      end;

    sCompress.Write( aDecompressed[ 0 ], Length( aDecompressed ) );
    SetLength( aDecompressed, 0 );
    if SaveDebugFiles then
      sCompress.SaveToFile( ChangeFileExt( FileName, '.' + OodleAlgoName[ Algo ] ) );

    sOut := TMemoryStream.Create;
    sCompress.Position := 0;
    SetLength( aDecompressed, 0 );
    result := ExtractOodle( sCompress.Memory, sCompress.Size, aDecompressed );
    if ( result <= 0 ) then
      begin
      sOut.free;
      sCompress.free;
      result := -2;
      Exit;
      end;
    sOut.Write( aDecompressed[ 0 ], Length( aDecompressed ) );
    SetLength( aDecompressed, 0 );

    if ( sIn.Size <> sOut.Size ) OR NOT CompareMem( sIn.Memory, sOut.Memory, sIn.Size ) then
      result := -1;
    if SaveDebugFiles then
      sOut.SaveToFile( ChangeFileExt( FileName, '.extract' + OodleAlgoName[ Algo ] ) );

    sOut.free;
    sCompress.free;
  end;

  {$IFNDEF Win64}
  function PointerToPointer( Algo : TOodleAlgo ) : Int64;
  var
    sCompress, sOut : TMemoryStream;
    pDecompressed : Pointer;
  begin
    pDecompressed := nil;

    sCompress := TMemoryStream.Create;
    result := CompressOodle( sIn.Memory, sIn.Size, pDecompressed, Algo, Header );
    if ( result <= 0 ) then
      begin
      sCompress.free;
      result := -3;
      Exit;
      end;

    sCompress.Write( pDecompressed^, result );
    ReallocMem( pDecompressed, 0 );
    if SaveDebugFiles then
      sCompress.SaveToFile( ChangeFileExt( FileName, '.' + OodleAlgoName[ Algo ] ) );

    sOut := TMemoryStream.Create;
    sCompress.Position := 0;
    pDecompressed := nil;
    result := ExtractOodle( sCompress.Memory, sCompress.Size, pDecompressed );
    if ( result <= 0 ) then
      begin
      sOut.free;
      sCompress.free;
      result := -2;
      Exit;
      end;
    sOut.Write( PByte( pDecompressed )^, result );
    ReallocMem( pDecompressed, 0 );

    if ( sIn.Size <> sOut.Size ) OR NOT CompareMem( sIn.Memory, sOut.Memory, sIn.Size ) then
      result := -1;
    if SaveDebugFiles then
      sOut.SaveToFile( ChangeFileExt( FileName, '.extract' + OodleAlgoName[ Algo ] ) );

    sOut.free;
    sCompress.free;
  end;
  {$ENDIF Win64}
var
  i : TOodleAlgo;
begin
  result := -999;
  if NOT FileExists( FileName ) then
    Exit;

  sIn := TMemoryStream.Create;
  sIn.LoadFromFile( FileName );
  sIn.Position := 0;

  for i := Low( TOodleAlgo ) to High( TOodleAlgo ) do
    begin
    if ( i = oaNone ) then
      Continue;

    result := PointerToArray( i );
    case result of
    -999 : ShowMessage( 'PointerToArray: Invalid File (' + OodleAlgoName[ i ] + ')' );
      -3 : ShowMessage( 'PointerToArray: Failed compress (' + OodleAlgoName[ i ] + ')' );
      -2 : ShowMessage( 'PointerToArray: Failed decompress (' + OodleAlgoName[ i ] + ')' );
      -1 : ShowMessage( 'PointerToArray: Size invalid or CompareMem failed (' + OodleAlgoName[ i ] + ')' );
       0 : ShowMessage( 'PointerToArray: Failed to Extract (' + OodleAlgoName[ i ] + ')' );
//    else
//       ShowMessage( 'PointerToArray: OK' );
    end;
//    if ( result < 0 ) then
//      begin
//      sIn.Free;
//      Exit;
//      end;

    {$IFNDEF Win64}
    result := PointerToPointer( i );
    case result of
    -999 : ShowMessage( 'PointerToPointer: Invalid File (' + OodleAlgoName[ i ] + ')' );
      -3 : ShowMessage( 'PointerToPointer: Failed compress (' + OodleAlgoName[ i ] + ')' );
      -2 : ShowMessage( 'PointerToPointer: Failed decompress (' + OodleAlgoName[ i ] + ')' );
      -1 : ShowMessage( 'PointerToPointer: Size invalid or CompareMem failed (' + OodleAlgoName[ i ] + ')' );
       0 : ShowMessage( 'PointerToPointer: Failed to Extract (' + OodleAlgoName[ i ] + ')' );
//    else
//       ShowMessage( 'PointerToPointer: OK' );
    end;
//    if ( result < 0 ) then
//      begin
//      sIn.Free;
//      Exit;
//      end;
    {$ENDIF Win64}
    end;
  sIn.free;
end;
{$ENDIF TESTCASE}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$DEFINE section_IMPLEMENTATION}
{$I DynamicDLL.inc}
{$UNDEF section_IMPLEMENTATION}

end.
