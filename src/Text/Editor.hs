-- Copyright 2015 Peter Harpending
-- 
-- Licensed under the Apache License, Version 2.0 (the "License"); you
-- may not use this file except in compliance with the License.  You
-- may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.


-- | 
-- Module      : Text.Editor
-- Description : Open the user's @$EDITOR@
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 
-- You know when you run @git commit@, and a little editor pops up?
-- This is a Haskell library that does that.

module Text.Editor where

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Monoid
import           System.IO
import           System.IO.Temp
import           System.Process
import           System.Posix

-- == Porcelain functions =

-- |If you don't want to use ByteString, use this function.
-- 
-- >>> :t runUserEditorDWIM plainTemplate mempty
-- ByteString
-- >>> :t wrapStr <$> runUserEditorDWIM plainTemplate mempty
-- String
wrapStr :: ByteString -> String
wrapStr = unpack

-- |This is most likely the function you want to use. It takes a file
-- type template as an argument, along with what you want displayed
-- when the user opens the editor. It then runs the editor, and
-- returns the version of the text that the user modified.
-- 
-- @
-- runUserEditorDWIM templ initialContents = userEditorDefault _default_editor >>= \theEditor ->
--                                             runSpecificEditor theEditor templ initialContents
-- @
-- 
-- 
-- Examples:
-- 
-- >>> :set -XOverloadedStrings
-- >>> runUserEditorDWIM jsonTemplate "{\n\n}\n"
-- 
-- This will open up the user's @$EDITOR@ configured to edit JSON, and
-- with the initial text:
--  
-- @
-- {
-- }
-- @
-- 
-- There are a bunch of templates. See the "File-type extensions"
-- section. It's also trivially easy to make your own templates. Say
-- you want one for, I dunno, Python:
-- 
-- @
-- pythonTemplate = mkTemplate "py"
-- @
-- 
-- The argument to 'mkTemplate' should be the file extension you
-- want. In that case, I used @"py"@, because Python's file extension
-- is @.py@.
runUserEditorDWIM :: Template       -- ^Template for the file name
                  -> ByteString     -- ^Initial contents
                  -> IO ByteString  -- ^Resulting ByteString
runUserEditorDWIM templ initialContents = userEditorDefault _default_editor >>= \theEditor ->
                                            runSpecificEditor theEditor templ initialContents

-- |This is the same as above, it just takes a file as an argument
-- instead of the ByteString
-- 
-- @
-- runUserEditorDWIMFile templ fp = B.readFile fp >>=  runUserEditorDWIM templ
-- @
runUserEditorDWIMFile :: Template       -- ^Template for the file name
                      -> FilePath       -- ^File containing initial contents
                      -> IO ByteString  -- ^Resulting ByteString
runUserEditorDWIMFile templ fp = B.readFile fp >>=  runUserEditorDWIM templ

-- |This is likely the simplest function here. It opens up the user's editor,
-- and fetches a ByteString from it
-- 
-- @
-- runUserEditor = userEditorDefault _default_editor >>= \theEditor ->
--                   runSpecificEditor theEditor plainTemplate mempty
-- @
runUserEditor :: IO ByteString
runUserEditor = userEditorDefault _default_editor >>= \theEditor ->
                  runSpecificEditor theEditor plainTemplate mempty

-- |This is probably the second-simplest function.
-- 
-- 
runUserEditorWithTemplate :: Template       -- ^Template for the file name
                          -> IO ByteString  -- ^Resulting ByteString
runUserEditorWithTemplate templ = userEditorDefault _default_editor >>= \theEditor ->
                                    runSpecificEditor theEditor templ mempty

-- == File-type extensions ==
-- 
-- If you use one of the extensions below, you'll likely enable syntax
-- highlighting in the user's editor.

type Template = String

-- |Make a template 
-- 
-- @
-- mkTemplate ext = _ftempl <> "." <> ext
-- @
-- 
-- >>> mkTemplate "blah"
-- tmp.blah
-- 
mkTemplate :: String -> Template
mkTemplate ext = _ftempl <> "." <> ext

-- |File-type template for HTML
-- 
-- @
-- htmlTemplate = mkTemplate "html"
-- @
htmlTemplate :: Template
htmlTemplate = mkTemplate "html"

-- |File-type template for JSON
-- 
-- @
-- jsonTemplate = mkTemplate "json"
-- @
jsonTemplate :: Template
jsonTemplate = mkTemplate "json"

-- |File-type template for Markdown
-- 
-- @
-- markdownTemplate = mkTemplate "md"
-- @
markdownTemplate :: Template
markdownTemplate = mkTemplate "md"

-- |Older file-type template for Markdown
-- 
-- @
-- markdownTemplate = mkTemplate "markdown"
-- @
oldMarkdownTemplate :: Template
oldMarkdownTemplate = mkTemplate "markdown"

-- |File-type template for plain text
-- 
-- @
-- plainTemplate = mkTemplate "txt"
-- @
plainTemplate :: Template
plainTemplate = mkTemplate "txt"

-- |File-type template for XML
-- 
-- @
-- xmlTemplate = mkTemplate "xml"
-- @
xmlTemplate :: Template
xmlTemplate = mkTemplate "xml"

-- |Same as 'plainTemplate'
txtTemplate :: Template
txtTemplate = mkTemplate "txt"

-- |File-type template for YAML
-- 
-- @
-- yamlTemplate = mkTemplate "yaml"
-- @
yamlTemplate :: Template
yamlTemplate = mkTemplate "yaml"

-- == Plumbing =
-- |Open an editor. You probably don't want to use this function.
-- 
-- @
-- runSpecific editorName templ initialContents =
--   withSystemTempFile templ $ \filePath hdl -> do
--     hSetBinaryMode hdl True
--     hSetBuffering hdl NoBuffering
--     B.hPut hdl initialContents
--     callProcess editorName [filePath]
--     B.hGetContents hdl
-- @
-- 
runSpecificEditor :: String        -- ^Name of the editor.
                  -> Template      -- ^Template for the file name.
                  -> ByteString    -- ^Initial contents of the file.
                  -> IO ByteString -- ^Resulting ByteString.
runSpecificEditor editorName templ initialContents =
  withSystemTempFile templ $ \filePath hdl -> do
    hSetBinaryMode hdl True
    hSetBuffering hdl NoBuffering
    B.hPut hdl initialContents
    callProcess editorName [filePath]
    B.hGetContents hdl


-- |This uses 'getEnv' from "System.Posix" to attempt to
-- get the user's @$EDITOR@ variable.
-- 
-- @
-- userEditor = getEnv _editor
-- @
userEditor :: IO (Maybe String)
userEditor = getEnv _editor

-- |Wrapper around 'userEditor' that includes a fallback option if the
-- @$EDITOR@ variable doesn't exist.
-- @
-- userEditorDefault def = userEditor >>= \case
--                           Just e  -> pure e
--                           Nothing -> pure def
-- @
userEditorDefault :: String -- ^Default value if @$EDITOR@ is not found.
                  -> IO String
userEditorDefault def = userEditor >>= \case
                          Just e  -> pure e
                          Nothing -> pure def


-- == Internal variables ==
-- 
-- You are welcome to use these, but I can't see why you would want to.

-- |The default editor if no other editor is found
-- 
-- @
-- _default_editor = "nano"
-- @
_default_editor :: String
_default_editor = "nano"

-- |The variable we should search when finding the user's editor.
-- 
-- @
-- _editor = "EDITOR"
-- @
_editor :: String
_editor = "EDITOR"

-- |The standard filename template
-- 
-- @
-- _ftempl = "tmp"
-- @
_ftempl :: String
_ftempl = "tmp"
