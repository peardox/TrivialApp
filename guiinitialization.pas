unit GUIInitialization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState, CastleProgress,
  Forms, Controls, Graphics, Dialogs, CastleControl, MainGameUnit,
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras,
  X3DNodes, X3DFields, X3DTIme,
  CastleImages, CastleGLImages,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    Window: TCastleControlBase;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
  end;

var
  CastleForm: TCastleForm;

implementation
{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  {$ifdef darwin}
  WindowState := wsFullScreen;
  {$endif}
  GLIsReady := False;
  Profiler.Enabled := true;
  InitializeLog;
  Caption := 'Basic CGE Lazarus Application';
end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
end;

procedure TCastleForm.WindowOpen(Sender: TObject);
begin
  GLIsReady := True;
  TCastleControlBase.MainControl := Window;
  CastleApp := TCastleApp.Create(Application);
  TUIState.Current := CastleApp;
  Window.Container.UIScaling := usDpiScale;
  AppProgress := TAppProgress.Create;
  Progress.UserInterface := AppProgress;
end;

procedure TCastleForm.WindowClose(Sender: TObject);
var
  i: Integer;
begin
  FreeAndNil(AppProgress);
  for i:= 0 to Length(hallImages) - 1 do
    begin
      FreeAndNil(imgCache[i]);
    end;
  SetLength(imgCache, 0);
end;

end.

