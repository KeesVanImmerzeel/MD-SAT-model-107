library dsmodel107;
  {-Metamodel voor de N-belasting van het ondiepe grondwater, o.a. gebaseerd op
    de GHG. SC-dlo Technical Document 61, p.23-25, (1999)}

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{.$define test}

uses
  ShareMem,
  {$ifdef test} forms, {$endif} windows, SysUtils, Classes, LargeArrays,
  ExtParU, USpeedProc, uDCfunc,UdsModel, UdsModelS, xyTable, DUtils, uError,
  Math;

Const
  cModelID      = 107;  {-Uniek modelnummer}

  {-Beschrijving van de array met afhankelijke variabelen}
  cNrOfDepVar   = 1; {-Lengte van de array met afhankelijke variabelen}
  cLogNopp      = 1; {-10log Nopp (No3-Nconc in grondwater)}

  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;
  
  {-Variabelen die samenhangen met het aanroepen van het model vanuit de Shell}
  cnRP    = 6; {-Aantal RP-tijdreeksen die door de Shell moeten worden aange-
                 leverd (in de externe parameter Array EP
                 (element EP[ indx-1 ]))}
  cnSQ    = 0; {-Idem punt-tijdreeksen}
  cnRQ    = 0; {-Idem lijn-tijdreeksen}

  {-Beschrijving van het eerste element van de externe parameter-array (EP[cEP0])}
  cNrXIndepTblsInEP0 = 6;    {-Aantal XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;    {-Aantal Xdep-tables   in EP[cEP0]}
  {-Nummering van de xIndep-tabellen in EP[cEP0]. De nummers 0&1 zijn gereserveerd}
  cTb_MinMaxValKeys   = 2;
  cTb_DefaultPar1     = 3;
  cTb_DefaultPar2     = 4;
  cTb_DefaultPar3     = 5;

  {-Beschrijving van het tweede element van de externe parameter-array (EP[cEP1])}
  {-Opmerking: table 0 van de xIndep-tabellen is gereserveerd}
  {-Nummering van de xdep-tabellen in EP[cEP1]}
  cTb_GHG       = 0;
  cTb_Gewas     = 1;
  cTb_Bodem     = 2;
  cTb_kwwz7     = 3;
  cTb_NOver     = 4;
  cTb_NconcKwel = 5;

  {-Model specifieke fout-codes}
  cInvld_GHG       = -9600;
  cInvld_Bodem     = -9601;
  cInvld_Gewas     = -9602;
  cInvld_kwwz7     = -9603;
  cInvld_NOver     = -9604;
  cInvld_NconcKwel = -9605;

var
  Indx: Integer; {-Door de Boot-procedure moet de waarde van deze index worden ingevuld,
                   zodat de snelheidsprocedure 'weet' waar (op de externe parameter-array)
				   hij zijn gegevens moet zoeken}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties
				   (zie nDC) }
  {-Geldige range van key-/parameter/initiele waarden. De waarden van deze
    variabelen moeten worden ingevuld door de Boot-procedure}
  cMin_GHG, cMax_GHG,
  cMin_Bodem, cMax_Bodem,
  cMin_Gewas, cMax_Gewas: Integer;
  cMin_kwwz7, cMax_kwwz7,
  cMin_NOver, cMax_NOver,
  cMin_NconcKwel, cMax_NconcKwel: Double;
Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
	if ( nDC > 0 ) then
      ModelProfile.Free;
  end;
end;

Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile; var IErr: Integer );
{-Deze procedure verschaft de array met afgeleiden 'dydx',
  gegeven het tijdstip 'x' en
  de toestand die beschreven wordt door de array 'y' en
  de externe condities die beschreven worden door de 'external parameter-array EP'.
  Als er geen fout op is getreden bij de berekening van 'dydx' dan wordt in deze procedure
  de variabele 'IErr' gelijk gemaakt aan de constante 'cNoError'.
  Opmerking: in de array 'y' staan dus de afhankelijke variabelen, terwijl 'x' de
  onafhankelijke variabele is}
var
  Bodem, Gewas: Integer;          {-Sleutel-waarden voor de default-tabellen in EP[cEP0]}
  GHG, Kwwz7, Kwel7, NOver, NconcKwel, {-Parameter-waarden afkomstig van de Shell}
  c, c_Nover, c_Nkwel, c_kwwz7, c_Nover_kwwz7, {-Default parameter-waarden in EP[cEP0]}
  c_gewas, c_Nover_gewas,
  c_bodem, c_Nover_bodem, c_bodem_gewas,
  c_GHG,
  c_GHG_gewas,
  c_GHG_bodem,
  c_GHG_kwwz7,
  Labda,
  Nkwel: Double;             {-Afgeleide (berekende) parameter-waarden}
  i: Integer;

Function SetKeyAndParValues( var IErr: Integer ): Boolean;

  Function GetGHG( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_GHG ].EstimateY( x, Direction );
  end;

  Function GetBodem( const x: Double ): Integer;
  begin
    with EP[ indx-1 ].xDep do
      Result := Trunc( Items[ cTb_Bodem ].EstimateY( x, Direction ) );
  end;

  Function GetGewas( const x: Double ): Integer;
  begin
    with EP[ indx-1 ].xDep do
      Result := Trunc( Items[ cTb_Gewas ].EstimateY( x, Direction ) );
  end;

  Function GetKwwz7( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do {-converteer van m/jaar naar mm/jaar}
      Result := Items[ cTb_kwwz7 ].EstimateY( x, Direction )*1000;
  end;

  Function GetKwel7( const x: Double ): Double;
  begin
    Result := Max( GetKwwz7( x ), 0 );
  end;

  Function GetNOver( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_NOver ].EstimateY( x, Direction );
  end;

  Function GetNconcKwel( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_NconcKwel ].EstimateY( x, Direction );
  end;

begin {-Function SetKeyAndParValues}
  Result := False;
  IErr   := cNoError;

  GHG := GetGHG( x );
  if ( GHG < cMin_GHG ) or ( GHG > cMax_GHG ) then begin
    IErr := cInvld_GHG; Exit;
  end;

  Gewas := GetGewas( x );
  if ( Gewas < cMin_Gewas ) or ( Gewas > cMax_Gewas ) then begin
    IErr := cInvld_Gewas; Exit;
  end;

  Bodem := GetBodem( x );
  if ( Bodem < cMin_Bodem ) or ( Bodem > cMax_Bodem ) then begin
    IErr := cInvld_Bodem; Exit;
  end;

  Kwwz7 := GetKwwz7( x );
  if ( Kwwz7 < cMin_kwwz7 ) or ( Kwwz7 > cMax_kwwz7 ) then begin
    IErr := cInvld_kwwz7; Exit;
  end;

  Kwel7 := GetKwel7( x );

  NOver := GetNOver( x );
  if ( NOver < cMin_NOver ) or ( NOver > cMax_NOver ) then begin
    IErr := cInvld_NOver; Exit;
  end;

  NconcKwel := GetNconcKwel( x );
  if ( NconcKwel < cMin_NconcKwel ) or ( NconcKwel > cMax_NconcKwel ) then begin
    IErr := cInvld_NconcKwel; Exit;
  end;

  {-Default parameter-waarden in EP[cEP0]}
  with EP[ cEP0 ].xInDep.Items[ cTb_DefaultPar1 ] do begin
    labda         := GetValue( 1, 1 ); {row, column}
    c             := GetValue( 1, 2 );
    c_Nover       := GetValue( 1, 3 );
    c_GHG         := GetValue( 1, 4 ); 
    c_Nkwel       := GetValue( 1, 5 );
    c_kwwz7       := GetValue( 1, 6 );
    c_Nover_kwwz7 := GetValue( 1, 7 );
    c_GHG_kwwz7   := GetValue( 1, 8 );
  end;

  with EP[ cEP0 ].xInDep.Items[ cTb_DefaultPar2 ] do begin
    c_gewas       := GetValue( Gewas, 1 ); {row, column}
    c_Nover_gewas := GetValue( Gewas, 2 );
    c_GHG_gewas   := GetValue( Gewas, 3 );
  end;

  with EP[ cEP0 ].xInDep.Items[ cTb_DefaultPar3 ] do begin
    c_bodem       := GetValue( Bodem, 1 ); {row, column}
    c_Nover_bodem := GetValue( Bodem, 2 );
    c_GHG_bodem   := GetValue( Bodem, 3 );
    c_bodem_gewas := GetValue( Bodem, 3 + Gewas );
  end;

  Nkwel := Kwel7 * NconcKwel / 100; {-kg/ha?}

  Result := True;
end; {-Function SetKeyAndParValues}

begin

  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;

  if ( Context = UpdateYstart ) then begin {-Run fase 1}

    {-Bij Shell-gebruik van het model (indx = cBoot2) dan kan het wenselijk zijn de tijd-as
	  van alle Shell-gegevens te converteren, bijvoorbeeld naar jaren}
    if ( indx = cBoot2 ) then
      ScaleTimesFromShell( cFromDayToYear, EP );

    IErr := cNoError;
  end else begin {-Run fase 2}

    if not SetKeyAndParValues( IErr ) then
      exit;

  {$ifdef test}
  Application.MessageBox( PChar(
    'labda: ' + FloatToStr( labda ) + #10 +
    'c: ' + FloatToStr( c ) + #10 +
    'c_Nover: ' + FloatToStr( c_Nover ) + #10 +
    'c_GHG: ' + FloatToStr( c_GHG ) + #10 +
    'c_Nkwel: ' + FloatToStr( c_Nkwel ) + #10 +
    'c_kwwz7: ' + FloatToStr( c_kwwz7 ) + #10 +
    'c_Nover_kwwz7: ' + FloatToStr( c_Nover_kwwz7 ) + #10 +
    'c_GHG_kwwz7: ' + FloatToStr( c_GHG_kwwz7 ) + #10 +
    'c_gewas: ' + FloatToStr( c_gewas ) + #10 +
    'c_Nover_gewas: ' + FloatToStr( c_Nover_gewas ) + #10 +
    'c_GHG_gewas: ' + FloatToStr( c_GHG_gewas ) + #10 +
    'c_bodem: ' + FloatToStr( c_bodem ) + #10 +
    'c_Nover_bodem: ' + FloatToStr( c_Nover_bodem ) + #10 +
    'c_GHG_bodem: ' + FloatToStr( c_GHG_bodem ) + #10 +
    'c_bodem_gewas: ' + FloatToStr( c_bodem_gewas ) + #10 +
    'Nkwel: ' + FloatToStr( Nkwel ) + #10 +
    'Nover: ' + FloatToStr( Nover ) + #10 +
    'Kwwz7: ' + FloatToStr( Kwwz7 ) ), 'Info', MB_OKCANCEL );
  {$endif}

    {-Bereken de array met afgeleiden 'dydx'}
    dydx[ cLogNopp ] := c+c_bodem+c_gewas+c_bodem_gewas+c_Nkwel*Nkwel+
      (c_GHG+c_GHG_bodem+c_GHG_gewas)*EXP(labda*GHG)+
      (c_kwwz7+c_GHG_kwwz7*EXP(labda*GHG)+c_Nover_kwwz7*Nover)*kwwz7+
      (c_Nover+c_Nover_bodem+c_Nover_gewas)*Nover;
  end;
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Initialiseer de meest elementaire gegevens van het model. Shell-gegevens worden door deze
    procedure NIET verwerkt}
Procedure SetMinMaxKeyAndParValues;
begin
  with EP[ cEP0 ].xInDep.Items[ cTb_MinMaxValKeys ] do begin
    cMin_GHG       := Trunc( GetValue( 1, 1 ) ); {rij, kolom}
    cMax_GHG       := Trunc( GetValue( 1, 2 ) );
    cMin_Gewas     := Trunc( GetValue( 1, 3 ) );
    cMax_Gewas     := Trunc( GetValue( 1, 4 ) );
    cMin_Bodem     := Trunc( GetValue( 1, 5 ) );
    cMax_Bodem     := Trunc( GetValue( 1, 6 ) );
    cMin_kwwz7     := 1000 * GetValue( 1, 7 ); {-converteer van m/jr naar mm/jr}
    cMax_kwwz7     := 1000 * GetValue( 1, 8 ); {-Idem}
    cMin_NOver     :=        GetValue( 1, 9 );
    cMax_NOver     :=        GetValue( 1, 10 );
    cMin_NconcKwel :=        GetValue( 1, 11 );
    cMax_NconcKwel :=        GetValue( 1, 12 );
  end;
end;
Begin
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC, cNrXIndepTblsInEP0, 
                                       cNrXdepTblsInEP0, Indx, EP );
  if ( Result = cNoError ) then
    SetMinMaxKeyAndParValues;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze boot-procedure verwerkt alle basisgegevens van het model en leest de Shell-gegevens
    uit een bestand. Na initialisatie met deze boot-procedure is het model dus gereed om
	'te draaien'. Deze procedure kan dus worden gebruikt om het model 'los' van de Shell te
	testen}
Begin
  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then 
    exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx, EP );
  if ( Result <> cNoError ) then 
    exit;
  SetReadyToRun( EP);
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze procedure maakt het model gereed voor Shell-gebruik.
    De xDep-tables in EP[ indx-1 ] worden door deze procedure NIET geinitialiseerd omdat deze
	gegevens door de Shell worden verschaft }
begin
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}

begin
  {-Dit zgn. 'DLL-Main-block' wordt uitgevoerd als de DLL voor het eerst in het geheugen wordt
    gezet (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );
end.
