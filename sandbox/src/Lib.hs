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
import qualified Data.Text as T

exports :: GdnativeHandle -> IO ()
exports desc = do
  registerClass $ RegClass desc $ classInit @MySprite

data MySprite =
  MySprite {
    _mBase :: GodotSprite,
    _mSpeed :: TVar Float,
    _mAngularSpeed :: TVar Float
  }

instance HasBaseClass MySprite where
  type BaseClass MySprite = GodotSprite
  super = _mBase

instance NativeScript MySprite where
  classInit base = do
    godotPrint "Hello, world!"
    MySprite base <$> newTVarIO 400 <*> newTVarIO pi

  classMethods =
    [
    func NoRPC "_process" $ \self -> \case
      [deltaVt] -> do
        delta <- fromGodotVariant @Float deltaVt
        angularSpeed <- atomically $ readTVar (_mAngularSpeed self)
        rotation <- get_rotation self
        set_rotation self (rotation + delta * angularSpeed)
      _ -> error "Unexpected number of args."
    ]

godotPrint :: Text -> IO ()
godotPrint = void . Api.godot_print <=< toLowLevel
