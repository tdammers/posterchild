{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main
where

import Database.Posterchild
import Database.Posterchild.SchemaConstraints
import Database.Posterchild.Parser
import Database.Posterchild.TH
import Database.Posterchild.Driver.Class
import Data.HList

$(mkSelectQueryDec "selectExample" "SELECT foo FROM bar WHERE 1")

main = return ()
