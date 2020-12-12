unit MainGameUnit;

{$mode objfpc}{$H+}
// {$define logall}
// {$define oneshot}

interface

uses
  Classes, SysUtils, Math, CastleUIState, CastleProgress,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras,
  X3DNodes, X3DFields, X3DTIme,
  CastleImages, CastleGLImages,
  CastleTextureImages, CastleCompositeImage, // For Caching textures
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  { ProgressInterface }
  TAppProgress = class(TProgressNullInterface)
    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
    function  Motion(const Event: TInputMotion): Boolean; override; // TUIState
    function  Press(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  Release(const Event: TInputPressRelease): Boolean; override; // TUIState
  private
    PointlessButton: TCastleButton;
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    LabelFPS: TCastleLabel;
    LabelProgress: TCastleLabel;
    LabelCamPos: TCastleLabel;
    LabelCamDir: TCastleLabel;
    LabelCamUp: TCastleLabel;
    LabelRender: TCastleLabel;
    LabelSceneLoad: TCastleLabel;
  public
    procedure PointlessButtonClick(Sender: TObject);
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil);
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
    procedure LoadScene(filename: String);
  end;

var
  AppTime: Int64;
  PrepDone: Boolean;
  ProgSteps: Cardinal;
  CastleApp: TCastleApp;
  AppProgress: TAppProgress;
  RenderReady: Boolean;

const
  RotateScene: Boolean = False;
  SecsPerRot: Single = 12;
  SceneFile: String = 'castle-data:/box_roty.x3dv';

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

procedure TAppProgress.Update(Progress: TProgress);
begin
  // Investigate CastleSceneCode #7767 + CastleScene #1216
  CastleApp.LabelProgress.Caption := 'Progress : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ' + IntToStr(Progress.Position) + ' of ' + IntToStr(Progress.Max);
  WriteLnLog('Progress Update : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ' + IntToStr(Progress.Position) + ' of ' + IntToStr(Progress.Max));
  {$ifndef cgeapp}
  Application.ProcessMessages;
  {$else}
  Application.ProcessAllMessages;
  {$endif}
  inherited;
end;

procedure TAppProgress.Init(Progress: TProgress);
begin
  CastleApp.LabelProgress.Caption := 'Progress : Starting';
  WriteLnLog('Progress Init : ' + IntToStr(Progress.Position) + ' of ' + IntToStr(Progress.Max));
  {$ifndef cgeapp}
  Application.ProcessMessages;
  {$else}
  Application.ProcessAllMessages;
  {$endif}
  inherited;
end;

procedure TAppProgress.Fini(Progress: TProgress);
begin
  CastleApp.LabelProgress.Caption := 'Progress : Completed ' + IntToStr(ProgSteps) + ' steps';
  WriteLnLog('Progress Fini : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ' + IntToStr(Progress.Position) + ' of ' + IntToStr(Progress.Max));
  {$ifndef cgeapp}
  Application.ProcessMessages;
  {$else}
  Application.ProcessAllMessages;
  {$endif}
  inherited;
end;

procedure TCastleApp.PointlessButtonClick(Sender: TObject);
var
  ProcTimer: Int64;
begin
  PointlessButton.Exists := False;
  ProcTimer := CastleGetTickCount64;
  LoadScene(SceneFile);
  ProcTimer := CastleGetTickCount64 - ProcTimer;
  WriteLnLog('ProcTimer (LoadScene) = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds');
  LabelSceneLoad.Caption := 'LoadScene = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds';
end;

procedure TCastleApp.CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil);
begin
  objButton := TCastleButton.Create(Application);
  objButton.Caption := ButtonText;
  objButton.Anchor(hpMiddle, 10);
  objButton.Anchor(vpBottom, 10 + (Line * 35));
  objButton.onClick := ButtonCode;
  InsertFront(objButton);
end;

procedure TCastleApp.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
begin
  objLabel := TCastleLabel.Create(Application);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objLabel);
end;

procedure TCastleApp.LoadViewport;
begin
  WriteLnLog('LoadViewport #1 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  // Set up the main viewport
  Viewport := TCastleViewport.Create(Application);
  // Use all the viewport
  Viewport.FullSize := true;
  // Automatically position the camera
  Viewport.AutoCamera := True;
  // Use auto navigation keys
  Viewport.AutoNavigation := True;

  // Add the viewport to the CGE control
  InsertFront(Viewport);

  CreateLabel(LabelCamPos, 0, False);
  CreateLabel(LabelCamDir, 1, False);
  CreateLabel(LabelCamUp, 2, False);

  CreateLabel(LabelSceneLoad, 3);
  CreateLabel(LabelProgress, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
  CreateButton(PointlessButton, 'The Completely Pointless Load Botton', 5, @PointlessButtonClick);
  WriteLnLog('LoadViewport #2 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.LoadScene(filename: String);
var
  ProfileStart: TCastleProfilerTime;
begin
  WriteLnLog('LoadScene #1 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  Progress.UpdateDelay := 0.1;
  Progress.UpdatePart := 30;
  Progress.Init(30, 'Preparing Scene');
  try
    Progress.step;
    try
      WriteLnLog('LoadScene #2 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
      ProfileStart := Profiler.Start('Scene loading profile - ' + filename);
      Progress.step;
      WriteLnLog('LoadScene #3 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
      Progress.Step; // So it's called at least once
      WriteLnLog('LoadScene #4 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');

      Scene := TCastleScene.Create(Application);
      Progress.step;
      WriteLnLog('LoadScene #5 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
      Scene.Spatial := [ssStaticCollisions, ssDynamicCollisions, ssRendering];
      Progress.step;
      WriteLnLog('LoadScene #6 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
      Scene.Load(filename);
      Progress.step;
      WriteLnLog('LoadScene #7 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
      Scene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
          True,
          Viewport.PrepareParams);
      Progress.step;
      WriteLnLog('LoadScene #8 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
      ProgSteps := Scene.PrepareResourcesSteps;
      WriteLnLog('LoadScene #9 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');

      // Add the scene to the viewport
      Viewport.Items.Add(Scene);
      Progress.step;
      WriteLnLog('LoadScene #10 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');

      // Tell the control this is the main scene so it gets some lighting
      Viewport.Items.MainScene := Scene;
      Progress.step;
      WriteLnLog('LoadScene #11 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');

      Progress.step;
      WriteLnLog('LoadScene #12 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');

      Profiler.Stop(ProfileStart, True);
      Progress.step;
      WriteLnLog('LoadScene #13 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
    except
      on E : Exception do
        begin
          WriteLnLog('Oops #1' + LineEnding + E.ClassName + LineEnding + E.Message);
         end;
    end;
  finally
    Progress.Fini;
  end;
  WriteLnLog('LoadScene #14 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.Start;
begin
  {$ifdef logall}
  WriteLnLog('UI.Start : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
  inherited;
  LogTextureCache := True;
  WriteLnLog('Start : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  Scene := nil;
  LoadViewport;
  PrepDone := True;
end;

procedure TCastleApp.Stop;
begin
  {$ifdef logall}
  WriteLnLog('UI.Stop : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
  inherited;
  WriteLnLog('Stop : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.BeforeRender;
var
  theta: Single;
  Pos, Dir, Up: TVector3;
begin
  {$ifdef logall}
  WriteLnLog('BeforeRender : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
  inherited;
  LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', Container.Fps.RealFps);
  LabelRender.Caption := 'Render = ' + FormatFloat('####0.00', Container.Fps.OnlyRenderFps);

  if not(Scene = nil) then
    begin
    Viewport.Camera.GetView(Pos, Dir, Up);

    LabelCamPos.Caption := 'Cam Pos : ' +
      FormatFloat('####0.00', Pos.X) + ', ' +
      FormatFloat('####0.00', Pos.Y) + ', ' +
      FormatFloat('####0.00', Pos.Z);

    LabelCamDir.Caption := 'Cam Dir : ' +
      FormatFloat('####0.00', Dir.X) + ', ' +
      FormatFloat('####0.00', Dir.Y) + ', ' +
      FormatFloat('####0.00', Dir.Z);

    LabelCamUp.Caption := 'Cam Up : ' +
      FormatFloat('####0.00', Up.X) + ', ' +
      FormatFloat('####0.00', Up.Y) + ', ' +
      FormatFloat('####0.00', Up.Z);

      if RotateScene then
        begin
          // Set angle (theta) to revolve completely once every SecsPerRot
          theta := ((CastleGetTickCount64 mod
                    (SecsPerRot * 1000)) /
                    (SecsPerRot * 1000)) * (Pi * 2);

          // Rotate the scene in Y
          // Change to Vector4(1, 0, 0, theta); to rotate in X

          Scene.Rotation := Vector4(0, 1, 0, theta);
      end;
    end;
end;

procedure TCastleApp.Render;
begin
  {$ifdef logall}
  WriteLnLog('Render : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
  inherited;

  if PrepDone and GLInitialized and RenderReady then
    begin
      PrepDone := False;
      PointlessButtonClick(nil);
      WriteLnLog('Scene Loaded (displayed?) : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000));
      {$ifdef oneshot}
      Application.Terminate;
      {$endif}
    end;
  RenderReady := True;
end;

procedure TCastleApp.Resize;
begin
  inherited;
  {$ifdef logall}
  WriteLnLog('Resize : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  {$ifdef logall}
  WriteLnLog('Update : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
end;

function TCastleApp.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  {$ifdef logall}
  WriteLnLog('Motion : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
end;

function TCastleApp.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  {$ifdef logall}
  WriteLnLog('Press : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
end;

function TCastleApp.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  {$ifdef logall}
  WriteLnLog('Release : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$endif}
end;

end.

