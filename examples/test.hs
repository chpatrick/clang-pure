import Clang
import Control.Lens

main = do
  idx <- createIndex
  tu <- parseTranslationUnit idx "examples/test.c" []
  let
    allNodes = universeOf cursorChildren $ translationUnitCursor tu
    cursorFilename = fmap (fileName . file . spellingLocation  . rangeStart) . cursorExtent
  print $ map cursorFilename allNodes
