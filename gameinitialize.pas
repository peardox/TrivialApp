unit GameInitialize;

{$mode objfpc}{$H+}

interface

uses
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleUIState, 
  CastleTimeUtils, CastleApplicationProperties, MainGameUnit;

procedure ApplicationInitialize;

implementation

var
  Window: TCastleWindowBase;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  GLIsReady := true;
  CastleApp := TCastleApp.Create(Application);
  TUIState.Current := CastleApp;
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'Basic';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization.

    For programs, InitializeLog is not called here.
    Instead InitializeLog is done by the program main file,
    after command-line parameters are parsed. }
  if IsLibrary then
    InitializeLog;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Window.Caption := 'Basic CGE Standalone Application';
  Application.MainWindow := Window;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.

