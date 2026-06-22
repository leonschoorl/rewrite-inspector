{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Basic functionality for the terminal user-inteface (TUI).
-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module BrickUI where

import Prelude hiding (fail)

import System.Environment     (getArgs)
import Control.Applicative    ((<|>))
import Control.Monad          (void,when)
import qualified Control.Monad.State    as State
import Control.Monad.IO.Class (liftIO)

import Data.Either         (fromRight)
import Data.List           ((!?), sortOn)
import Data.Maybe          (listToMaybe, catMaybes)
import Lens.Micro
import Lens.Micro.Mtl ((.=),(%=), use)

import Brick
  ( App (..), BrickEvent (..), EventM, Widget (..)
  , CursorLocation (..), cursorLocationName, cursorsL
  , VisibilityRequest (..), visibilityRequestsL
  , hSize, vSize
  , halt
  , str, vBox, hBox
  )
import Brick.Focus  (focusRingCursor, focusGetCurrent)
import Brick.Themes (loadCustomizations, themeToAttrMap)
import qualified Brick                as B
import qualified Brick.Forms          as Bf
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty         as V

import Gen
import Types
import Pretty

-- | Entry point for the TUI.
runTerminal :: forall term. Diff term => FilePath -> IO ()
runTerminal ftheme = do
  let dtheme = defaultTheme (userStyles @term)
  theme <- fromRight dtheme <$> loadCustomizations ftheme dtheme
  args <- getArgs
  case args of
    [fname] -> do
      hist <- readHistory @term fname
      void $ B.defaultMain
        (app @term (themeToAttrMap theme))  -- the Brick application
        (createVizStates @term hist)        -- initial state
    _ -> error "Usage: clash-term <history_file>"

-- | The 'Brick.App' configuration.
app :: Diff term => B.AttrMap -> App (VizStates term) NoCustomEvent Name
app attrMap = App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleStart
  , appStartEvent   = enableMouse >> lookupSize
  , appAttrMap      = const attrMap
  }

enableMouse :: EventM n s ()
enableMouse = do
  vty <- B.getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
      liftIO $ V.setMode output V.Mouse True

-- | Choose a single cursor to display, out of possibly many requests.
chooseCursor :: VizStates term -> [Cursor] -> Maybe Cursor
chooseCursor st ls
  =  (listToMaybe $ filter isSearch ls)
 <|> (focusRingCursor (Bf.formFocus . _form) st ls)
  where
    isSearch :: Cursor -> Bool
    isSearch = \case
      CursorLocation {cursorLocationName = Just (SearchResult _)} -> True
      _                                                           -> False

-- * Display.

-- Draw all top-level binders and their current step.
drawUI :: forall term. Diff term
       => VizStates term -> [Widget Name]
drawUI vs =
  [ B.translateBy (B.Location controlsOffset) controls
  | vs^.showCtrls
  ]
  ++
  [ vBox
      [ B.vLimitPercent 20 $
          C.hCenter $
            hBoxSpaced 2 (drawBndr <$> vs^.binders)
      , diff
      , B.vLimitPercent 10 $
          hBox
            [ inputForm
            , searchMatches
            ]
      ]
  ]
  where
    -- display the top-level binders
    drawBndr :: Binder -> Widget Name
    drawBndr bndr
      = wb (show curN ++ "/" ++ show totN ++ " (" ++ stepName ++ ")")
      $ str (fillSize 50 bndr)
      where
        (curN, totN, stepName) = getStep vs bndr
        wb | bndr == vs^.curBinder = withBorderSelected
           | otherwise             = withBorder

    -- display the diff of this rewrite step
    diff :: Widget Name
    diff
      | v@(VizState (st:_) _ curE _ curO _ _) <- getCurrentState vs
      = let
          showE = showCode (vs^.scroll)
                           (min 80 $ getCodeWidth vs)
                           (vs^.formData^.opts)
                           (st^.ctx)
                           (getSearchString vs)
          nextE = step v ^. curExpr
          (visL, visR) | v^.curOccur < v^.leftN
                       = (visibleCursors curO, invisibleCursors)
                       | otherwise
                       = (invisibleCursors, visibleCursors (curO - v^.leftN))
        in
          hBoxSpaced 2
            [ B.viewport LeftViewport B.Both $
                visL $
                  withBorder "Before" $ showE curE
            , B.viewport RightViewport B.Both $
                visR $
                  withBorder "After" $ showE nextE
            ]

      | otherwise
      = C.center $ title "THE END"

    -- display the (editable) input form
    inputForm :: Widget Name
    inputForm = withBorder "Input"
              $ Bf.renderForm
              $ Bf.setFormConcat (hBoxSpaced 10)
              $ vs^.form

    searchMatches :: Widget Name
    searchMatches = C.vCenter $ str (n ++ " out of " ++ tot ++ " matches")
      where
        (n, tot)
          | v@(VizState (_:_) _ _ _ _ _ _) <- getCurrentState vs
          , let lr = v^.leftN + v^.rightN
          , lr > 0
          = (show (v^.curOccur + 1), show lr)
          | otherwise
          = ("-", "-")

    -- display the keyboard controls
    controlsOffset :: (Int, Int)
    controlsOffset = ( (vs^.width `div` 2) - 25
                     , (vs^.height `div` 2) - 15
                     )

    controls :: Widget Name
    controls = withBorder "Controls" $ vBoxSpaced
      [ "→ / ← (Ctrl-l / Ctrl-k)" .- "next/previous binder"
      , "↓ / ↑"                   .- "next/previous step"
      , "r"                       .- "reset"
      , "Escape / q"              .- "quit"
      , "Shift-<dir>"             .- "scroll left pane"
      , "Ctrl-<dir>"              .- "scroll right pane"
      , "PageUp/Down"             .- "scroll both panes (up/down)"
      , "Home/End"                .- "(vertically) scroll to start/end"
      , "Ins/Del"                 .- "scroll both panes (left/right)"
      , "Ctrl-p / h / ?"          .- "show/hide keyboard controls"
      , "(Shift-)Tab"             .- "cycle through input fields"
      , "Enter"                   .- "submit move action (forward)"
      , "KBS/Ctrl-b"              .- "submit move action (backward)"
      , "Space"                   .- "toggle flag"
      , "1 - 9"                   .- "toggle flag 1 - 9"
      ]
      where
        button .- desc = hBox [emph button, str $ " : " ++ desc]

-- * Event handling.

-- | Lookup terminal size and store in the current state.
lookupSize :: EventM Name (VizStates term) ()
lookupSize = do
  out    <- V.outputIface <$> B.getVtyHandle
  (w, h) <- liftIO (V.displayBounds out)
  width .= w
  height .= h

-- | Update number of occurrences of searched string in both viewports.
updateOcc :: Diff term => VizStates term -> VizStates term
updateOcc vs
  | v@(VizState (_:_) _ curE _ _ _ _) <- getCurrentState vs
  , let ln = countOcc (vs^.formData^.opts) (getSearchString vs) curE
        rn = countOcc (vs^.formData^.opts) (getSearchString vs) (step v ^. curExpr)
  , ln + rn > 0
  = updateState vs $ v & leftN    .~ ln
                       & rightN   .~ rn
                       & curOccur .~ ((v^.curOccur) `mod` (ln + rn))

  | otherwise
  = vs

-- | Lookup code sizes and store them in the current state, then handle events.
handleStart :: forall term. Diff term
            => BrickEvent Name NoCustomEvent
            -> EventM Name (VizStates term) ()
handleStart ev = do
  vs <- State.get -- TODO remove?
  handleEvent vs ev
  State.modify updateOcc

-- | Handle keyboard events.
handleEvent :: forall term. Diff term
            => VizStates term
            -> BrickEvent Name NoCustomEvent
            -> EventM Name (VizStates term) ()
handleEvent vs ev@(VtyEvent (V.EvKey key mods))

  -- some controls are disabled when the user is writing in the input form
  | [] <- mods
  , focusGetCurrent (Bf.formFocus (vs^.form)) /= Just (FormField "Command")
  = sometimes

  -- the rest of the controls are active all the time
  | [] <- mods
  = always

  | [V.MShift] <- mods
  = shiftScroll

  | [V.MCtrl] <- mods
  = case key of
      -- show/hide controls pane
      V.KChar 'p' -> showCtrls %= not
      -- action (forward)
      V.KChar 'b' -> action Backward
      -- change top-level binder
      V.KChar 'l' -> contT stepBinder
      V.KChar 'k' -> contT unstepBinder
      _        -> ctrlScroll

  | otherwise
  = B.continueWithoutRedraw

  where
    contT :: (VizStates term -> VizStates term) -> EventM n (VizStates term) ()
    contT upd  = scroll .= True >> State.modify upd
    contF :: EventM Name (VizStates term) a -> EventM Name (VizStates term) ()
    contF      = (>> (scroll .= False))
    bottom :: (VizState term -> VizState term)
                      -> EventM n (VizStates term) ()
    bottom fg  = State.put $ updateState vs (fg $ getCurrentState vs)
                          & scroll .~ True
    action :: Direction -> EventM n (VizStates term) ()
    action dir  = case vs^.formData.com of
      Step n   -> bottom $ moveTo n
      Trans s  -> bottom $ nextTrans dir s
      Search _ -> bottom $ nextOccur dir

    sometimes = case key of
      -- reset to initial step (of current binder)
      V.KChar 'r' -> bottom reset
      -- move to previous step/transformation
      V.KBS       -> action Backward
      -- change top-level binder
      V.KRight    -> contT stepBinder
      V.KLeft     -> contT unstepBinder
      V.KChar c | '1' <= c && c <= '9' -> toggleFlag c
      _           -> always

    toggleFlag c = case flagFields @term !? n of
      Nothing -> return ()
      Just (g,s,_) -> (formData . opts . lens g s) %= not
      where
        n = read @Int [c] - 1

    always = case key of
      -- basic controls
      V.KEsc      -> halt
      V.KChar 'q' -> halt
      -- change step of current binder
      V.KDown     -> bottom step
      V.KUp       -> bottom unstep
      -- vertical scrolling (both)
      V.KPageDown -> contF (vScrollL     >> vScrollR)
      V.KPageUp   -> contF (vScrollL'    >> vScrollR')
      V.KHome     -> contF (vScrollHomeL >> vScrollHomeR)
      V.KEnd      -> contF (vScrollEndL  >> vScrollEndR)
      -- horizontal scrolling (both)
      V.KDel      -> contF (hScrollL  >> hScrollR)
      V.KIns      -> contF (hScrollL' >> hScrollR')
      -- move to next step/transformation
      V.KEnter    -> action Forward
      -- show controls
      V.KChar '?' -> showCtrls %= not
      V.KChar 'h' -> showCtrls %= not

      -- dispatch to form handler
      _           -> formHandler

    shiftScroll = contF $ case key of
      -- vertical/horizontal scrolling (left side)
      V.KDown  -> vScrollL
      V.KUp    -> vScrollL'
      V.KRight -> hScrollL
      V.KLeft  -> hScrollL'
      _        -> return ()

    ctrlScroll = contF $ case key of
      -- vertical/horizontal scrolling (right side)
      V.KDown  -> vScrollR
      V.KUp    -> vScrollR'
      V.KRight -> hScrollR
      V.KLeft  -> hScrollR'
      _        -> return ()

    -- form-handler
    formHandler :: EventM Name (VizStates term) ()
    formHandler = do
      B.zoom form $ Bf.handleFormEvent ev
      fm' <- use form
      let cm          = (Bf.formState fm')^.com
          (_, tot, _) = getStep vs (vs^.curBinder)
          valid       = case cm of Step n  -> n > 0 && n <= tot
                                   _       -> True
      form .= Bf.setFieldValid valid (FormField "Command") fm'

handleEvent _ (VtyEvent (V.EvResize _ _)) = lookupSize

handleEvent _ ev@(MouseDown {}) = do
  B.zoom form $ Bf.handleFormEvent ev
handleEvent _ ev@(MouseUp {}) = do
  B.zoom form $ Bf.handleFormEvent ev

-- no-op event
handleEvent _ _ = B.continueWithoutRedraw

-- * Scrolling.

-- | The amount of scrolling with each request (in pixels).
scrollStep :: Int
scrollStep = 5

l, r :: B.ViewportScroll Name
l = B.viewportScroll LeftViewport
r = B.viewportScroll RightViewport

vScrollL, vScrollR, hScrollL, hScrollR, vScrollL', vScrollR', hScrollL', hScrollR',
  vScrollHomeL, vScrollHomeR, vScrollEndL, vScrollEndR :: EventM Name s ()
vScrollL     = B.vScrollBy l scrollStep
vScrollL'    = B.vScrollBy l (-scrollStep)
vScrollR     = B.vScrollBy r scrollStep
vScrollR'    = B.vScrollBy r (-scrollStep)
hScrollL     = B.hScrollBy l scrollStep
hScrollL'    = B.hScrollBy l (-scrollStep)
hScrollR     = B.hScrollBy r scrollStep
hScrollR'    = B.hScrollBy r (-scrollStep)
vScrollHomeL = B.vScrollToBeginning l
vScrollHomeR = B.vScrollToBeginning r
vScrollEndL  = B.vScrollToEnd l
vScrollEndR  = B.vScrollToEnd r

-- | Gather all cursor placement requests coming from within the given 'Widget',
-- filter out only those that are the result of a /search/ command,
-- and convert the current one (based on the current occurrence number)
-- to a visibility request.
-- NB: Only to be used within a 'viewport'.
visibleCursors :: Int -> Widget Name -> Widget Name
visibleCursors n p = Widget (hSize p) (vSize p) $ do
  res <- B.render p
  let crs  = map fst
           $ sortOn ((\case {SearchResult i -> i; _ -> 0}) . snd)
           $ catMaybes
           $ map (\c ->  case cursorLocationName c of
              Just s@(SearchResult _) -> Just (c, s)
              _                       -> Nothing)
           $ (res^.cursorsL)
  if null crs then
    return res
  else do
    let c = crs !! (n `mod` length crs)
    return $ res & visibilityRequestsL .~ [VR { vrPosition = cursorLocation c
                                              , vrSize     = (1, 1)
                                              }]
                 & cursorsL .~ [c]

-- | Remove all cursor placement requests coming from within the given 'Widget'.
-- NB: Only to be used within a 'viewport'.
invisibleCursors :: Widget n -> Widget n
invisibleCursors p = Widget (hSize p) (vSize p) $ do
  res <- B.render p
  return $ res & cursorsL .~ []
