unit MainGameUnit;

{$mode objfpc}{$H+}

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
  CastleImages,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  TCastleViewportHelper = class helper for TCastleViewport
  public
    function ViewportToWorld: TVector2;
    procedure SetCamera(const AWidth: Single = 1; const AHeight: Single = 1; const Depth: Single = 1.0);
  end;

  { ProgressNullInterface }
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
    WalkNavigation: TCastleWalkNavigation;
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
  GLIsReady: Boolean;
  ProgSteps: Cardinal;
  CastleApp: TCastleApp;
  AppProgress: TAppProgress;

const
  RotateScene: Boolean = False;
  SecsPerRot: Single = 12;
  SceneFile: String = 'castle-data:/exebition_hall/scene.gltf';

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

procedure TAppProgress.Update(Progress: TProgress);
begin
  inherited;
  CastleApp.LabelProgress.Caption := 'Progress : ' + IntToStr(Progress.Position) + ' of ' + IntToStr(Progress.Max);
  WriteLnLog('Step');
end;

procedure TAppProgress.Init(Progress: TProgress);
begin
  inherited;
  CastleApp.LabelProgress.Caption := 'Progress : Starting';
end;

procedure TAppProgress.Fini(Progress: TProgress);
begin
  inherited;
  CastleApp.LabelProgress.Caption := 'Progress : Completed ' + IntToStr(ProgSteps) + ' steps';
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
  // Set up the main viewport
  Viewport := TCastleViewport.Create(Application);
  // Use all the viewport
  Viewport.FullSize := true;
  // Automatically position the camera
  Viewport.AutoCamera := False;
  // Use walk navigation keys
  Viewport.AutoNavigation := False;
  WalkNavigation := TCastleWalkNavigation.Create(FreeAtStop);
  WalkNavigation.MoveSpeed := 2.0;
  Viewport.Navigation := WalkNavigation;

  // Add the viewport to the CGE control
  InsertFront(Viewport);

  CreateLabel(LabelCamPos, 0, False);
  CreateLabel(LabelCamDir, 1, False);
  CreateLabel(LabelCamUp, 2, False);
  CreateLabel(LabelProgress, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
  CreateButton(PointlessButton, 'The Completely Pointless Load Botton', 5, @PointlessButtonClick);
end;

procedure TCastleApp.LoadScene(filename: String);
var
  ProfileStart: TCastleProfilerTime;
begin
  Progress.Init(100, 'Preparing Scene');
  try
    try
      ProfileStart := Profiler.Start('Scene loading profile - ' + filename);
      Progress.Step; // So it's called at least once

      Scene := TCastleScene.Create(Application);
      Scene.Spatial := [ssStaticCollisions, ssDynamicCollisions, ssRendering];
      Scene.Load(filename);
      ProgSteps := Scene.PrepareResourcesSteps;
      Scene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
          True,
          Viewport.PrepareParams);

      // Add the scene to the viewport
      Viewport.Items.Add(Scene);

      // Tell the control this is the main scene so it gets some lighting
      Viewport.Items.MainScene := Scene;

      Viewport.SetCamera;

      Profiler.Stop(ProfileStart, True);
    except
      on E : Exception do
        begin
          WriteLnLog('Oops #2' + LineEnding + E.ClassName + LineEnding + E.Message);
         end;
    end;
  finally
    Progress.Fini;
  end;
end;

procedure TCastleApp.Start;
begin
  inherited;
  Scene := nil;
  LoadViewport;
end;

procedure TCastleApp.Stop;
begin
  inherited;
end;

procedure TCastleApp.BeforeRender;
var
  theta: Single;
  Pos, Dir, Up: TVector3;
begin
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
  inherited;
end;

procedure TCastleApp.Resize;
begin
  inherited;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

function TCastleApp.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

function    TCastleApp.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

function TCastleViewportHelper.ViewportToWorld: TVector2;
var
  Vect1, Vect2:  TVector3;
begin
  if not PositionToWorldPlane(Vector2(RenderRect.Left, RenderRect.Bottom), True, 0, Vect1) then
    Exit(Vector2(0, 0));

  if not PositionToWorldPlane(Vector2(RenderRect.Width, RenderRect.Height), True, 0, Vect2) then
    Exit(Vector2(0, 0));

  Result.X := Round((Max(Vect1.X, Vect2.X) - Min(Vect1.X, Vect2.X)) * 1000) / 1000;
  Result.Y := Round((Max(Vect1.Y, Vect2.Y) - Min(Vect1.Y, Vect2.Y)) * 1000) / 1000;

  if (Result.X = 0) or (Result.Y = 0) then
    Exit(Vector2(0, 0));

end;

procedure TCastleViewportHelper.SetCamera(const AWidth: Single = 1; const AHeight: Single = 1; const Depth: Single = 1.0);
var
  FOVA: TFieldOfViewAxis;
  Theta: Single;
  s2w: TVector2;
begin
  if not RenderRect.IsEmpty then
  begin
    if Depth <= 0 then
      Exit;

    s2w := ViewportToWorld;

    if s2w.IsZero then
      begin
        if(RenderRect.Width < RenderRect.Height) then
          s2w := Vector2(AWidth, AHeight)
        else
          s2w := Vector2(AHeight, AWidth)
      end;

    if (s2w.X > AWidth) and (s2w.Y > AHeight) then
      begin
        FOVA := faHorizontal;
        Theta := ArcTan2((1 / Depth), (1 / AWidth)) * 2;
      end
    else if (s2w.X < AWidth) then
      begin
        FOVA := faHorizontal;
        Theta := ArcTan2((1 / Depth), (1 / AWidth)) * 2;
      end
    else if (s2w.Y < AHeight) then
      begin
        FOVA := faVertical;
        Theta := ArcTan2((1 / Depth), (1 / AHeight)) * 2;
      end
    else  if (AWidth < s2w.X) then
      begin
        FOVA := faVertical;
        Theta := ArcTan2((1 / Depth), (1 / AHeight)) * 2;
      end
    else  if (AHeight < s2w.Y) then
      begin
        FOVA := faHorizontal;
        Theta := ArcTan2((1 / Depth), (1 / AWidth)) * 2;
      end
    else
      begin
        FOVA := faHorizontal;
        Theta := ArcTan2((1 / Depth), (1 / AWidth)) * 2;
      end;

    Camera.Perspective.FieldOfView := Theta;
    Camera.SetView(Vector3(0, 0, 1 / ((1 / Depth) * 2)), Vector3(0, 0, -1), Vector3(0, 1, 0));
    Camera.Perspective.FieldOfViewAxis := FOVA;
  end;
end;

end.

