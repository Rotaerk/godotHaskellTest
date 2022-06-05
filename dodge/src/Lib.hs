{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (
    exports
)
where

import Godot
import qualified Godot.Gdnative.Internal.Api as Api

import Control.Monad
import Linear.V2
import Linear.Vector

exports :: GdnativeHandle -> IO ()
exports desc = do
  registerClass $ RegClass desc $ classInit @Player

data Player =
  Player {
    _mBase :: GodotArea2D,
    _mSpeed :: TVar Float,
    _mScreenSize :: TVar (V2 Float)
  }

instance HasBaseClass Player where
  type BaseClass Player = GodotArea2D
  super = _mBase

instance NativeScript Player where
  classInit base = Player base <$> newTVarIO 400 <*> newTVarIO zero

  classMethods =
    [
    func NoRPC "_ready" $ \self -> \case
      _ -> do
        writeTVarIO (_mScreenSize self) <=< getRectSize <=< get_viewport_rect $ self
    ,
    func NoRPC "_process" $ \self -> \case
      [deltaVrnt] -> do
        delta <- fromGodotVariant @Float deltaVrnt
        return ()
      _ -> error "Unexpected number of args."
    ]

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO x = atomically . writeTVar x

getRectSize :: GodotRect2 -> IO (V2 Float)
getRectSize = fromLowLevel <=< Api.godot_rect2_get_size
