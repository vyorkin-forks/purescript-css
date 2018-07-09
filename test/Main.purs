module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Exception (error, throwException)
import CSS (FontFaceFormat(..), FontFaceSrc(..), Path(..), Predicate(..), Refinement(..), Rendered, Selector(..), a, before, black, block, blue, body, border, borderBox, boxSizing, byClass, byId, color, contentBox, cursor, dashed, deg, direction, display, em, fontFaceSrc, fontStyle, fromString, gold, hover, inlineBlock, olive, opacity, outline, p, px, red, render, renderedInline, renderedSheet, rgba, selector, solid, teal, textOverflow, violet, zIndex, ($=), (&), (*=), (?), (@=), (^=), (|*), (|+), (|=), (|>), (~=))
import CSS.FontStyle as FontStyle
import CSS.Text.Overflow as TextOverflow
import CSS.Cursor as Cursor
import CSS.Box (boxShadow, shadow, shadowWithBlur, shadowWithSpread, bsColor, bsInset)
import CSS.Text.Direction as TextDirection
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton, (:|))

example1 :: Rendered
example1 = render do
  color red
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3a :: Rendered
example3a = render do
  border dashed (px 2.0) blue

example3b :: Rendered
example3b = render do
  outline solid (px 1.0) violet

example4 :: Rendered
example4 = render do
  body ? do
    color blue
  fromString "#world" ? do
    display block

example5 :: Rendered
example5 = render do
  boxSizing contentBox
  boxSizing borderBox

example6 :: Rendered
example6 = render do
  fontFaceSrc $ singleton $ FontFaceSrcUrl "font.woff" $ Just WOFF

example7 :: Rendered
example7 = render do
  zIndex 11
  opacity 0.5

withSelector :: Rendered
withSelector = render do
  a & before ? do
    color blue
  a & hover ? do
    color red

childSelector :: Rendered
childSelector = render do
  p |> a ? do
    zIndex 9

deepSelector :: Rendered
deepSelector = render do
  p |* a ? do
    display block

byClassById :: Rendered
byClassById = render do
  a & byClass "bar" ? color red
  p & byId "foo" ? display block

attrVal :: Rendered
attrVal = render do
  p & ("foo" @= "bar") ? display block

attrBegins :: Rendered
attrBegins = render do
  p & ("foo" ^= "bar") ? display block

attrEnds :: Rendered
attrEnds = render do
  p & ("foo" $= "bar") ? display block

attrContains :: Rendered
attrContains = render do
  p & ("foo" *= "bar") ? display block

attrSpace :: Rendered
attrSpace = render do
  p & ("foo" ~= "bar") ? display block

attrHyph :: Rendered
attrHyph = render do
  p & ("foo" |= "bar") ? display block

adjacentSelector :: Rendered
adjacentSelector = render do
  a |+ a ? do
    display inlineBlock

exampleFontStyle1 :: Rendered
exampleFontStyle1 = render do
  fontStyle FontStyle.italic

exampleFontStyle2 :: Rendered
exampleFontStyle2 = render do
  fontStyle FontStyle.oblique

exampleFontStyle3 :: Rendered
exampleFontStyle3 = render do
  fontStyle $ FontStyle.obliqueAngle (deg 45.0)

exampleTextOverflow1 :: Rendered
exampleTextOverflow1 = render do
  textOverflow TextOverflow.ellipsis

exampleTextOverflow2 :: Rendered
exampleTextOverflow2 = render do
  textOverflow $ TextOverflow.custom "foobar"

exampleCursor :: Rendered
exampleCursor = render do
  cursor Cursor.notAllowed

singleShadow :: Rendered
singleShadow = render do
  boxShadow $ singleton $ bsColor teal $ shadow (px 60.0) (px (-16.0))

singleShadowWithBlur :: Rendered
singleShadowWithBlur = render do
  boxShadow $ singleton $ bsColor black $ shadowWithBlur (px 10.0) (px 5.0) (px 5.0)

singleShadowWithSpread :: Rendered
singleShadowWithSpread = render do
  boxShadow $ singleton $ rgba 0 0 0 0.2 `bsColor` shadowWithSpread (px 2.0) (px 2.0) (px 2.0) (px 1.0)

singleInsetShadow :: Rendered
singleInsetShadow = render do
  boxShadow $ singleton $ bsInset $ gold `bsColor` shadow (em 5.0) (em 1.0)

multipleShadows :: Rendered
multipleShadows = render do
  boxShadow $
    red `bsColor` shadow (px 3.0) (px 3.0) :|
    [olive `bsColor` shadowWithBlur (em (-1.0)) (em 0.0) (em 0.4)]

exampleDirection :: Rendered
exampleDirection = render do
  direction TextDirection.rtl

nestedNodes :: Rendered
nestedNodes = render do
  fromString "#parent" ? do
    display block
    fromString "#child" ? display block

nestedNodesWithEmptyParent :: Rendered
nestedNodesWithEmptyParent = render do
  fromString "#parent" ? do
    fromString "#child" ? display block

assertEqual :: forall a. Eq a => Show a => a -> a -> Effect Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Effect Unit
main = do
  renderedInline example1 `assertEqual` Just "color: hsl(0.0, 100.0%, 50.0%); display: block"
  renderedInline example2 `assertEqual` Just "display: inline-block"
  renderedInline example3a `assertEqual` Just "border: dashed 2.0px hsl(240.0, 100.0%, 50.0%) "
  renderedInline example3b `assertEqual` Just "outline: solid 1.0px hsl(300.0, 76.06%, 72.16%) "

  selector (Selector (Refinement [Id "test"]) Star) `assertEqual` "#test"

  selector (fromString "#test") `assertEqual` "#test"

  renderedSheet example4 `assertEqual` Just "body { color: hsl(240.0, 100.0%, 50.0%) }\n#world { display: block }\n"

  renderedInline example5 `assertEqual` Just "box-sizing: content-box; box-sizing: border-box"

  renderedSheet withSelector `assertEqual` Just "a::before { color: hsl(240.0, 100.0%, 50.0%) }\na:hover { color: hsl(0.0, 100.0%, 50.0%) }\n"
  renderedSheet childSelector `assertEqual` Just "p > a { z-index: 9 }\n"
  renderedSheet deepSelector `assertEqual` Just "p a { display: block }\n"
  renderedSheet adjacentSelector `assertEqual` Just "a + a { display: inline-block }\n"

  renderedSheet nestedNodes `assertEqual` Just "#parent { display: block }\n#parent #child { display: block }\n"

  renderedSheet nestedNodesWithEmptyParent `assertEqual` Just "#parent #child { display: block }\n"

  renderedInline example6 `assertEqual` Just "src: url(\"font.woff\") format(\"woff\")"

  renderedInline example7 `assertEqual` Just "z-index: 11; opacity: 0.5"

  renderedInline exampleFontStyle1 `assertEqual` Just "font-style: italic"
  renderedInline exampleFontStyle2 `assertEqual` Just "font-style: oblique"
  renderedInline exampleFontStyle3 `assertEqual` Just "font-style: oblique 45.0deg"

  renderedInline exampleTextOverflow1 `assertEqual` Just "text-overflow: ellipsis"
  renderedInline exampleTextOverflow2 `assertEqual` Just "text-overflow: \"foobar\""

  renderedSheet byClassById `assertEqual` Just "a.bar { color: hsl(0.0, 100.0%, 50.0%) }\np#foo { display: block }\n"
  renderedSheet attrVal `assertEqual` Just "p[foo='bar'] { display: block }\n"
  renderedSheet attrBegins `assertEqual` Just "p[foo^='bar'] { display: block }\n"
  renderedSheet attrEnds `assertEqual` Just "p[foo$='bar'] { display: block }\n"
  renderedSheet attrContains `assertEqual` Just "p[foo*='bar'] { display: block }\n"
  renderedSheet attrSpace `assertEqual` Just "p[foo~='bar'] { display: block }\n"
  renderedSheet attrHyph `assertEqual` Just "p[foo|='bar'] { display: block }\n"

  renderedInline exampleCursor `assertEqual` Just "cursor: not-allowed"

  renderedInline singleShadow `assertEqual` Just "-webkit-box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%); -moz-box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%); -ms-box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%); -o-box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%); box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%)"
  renderedInline singleShadowWithBlur `assertEqual` Just "-webkit-box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%); -moz-box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%); -ms-box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%); -o-box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%); box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%)"
  renderedInline singleShadowWithSpread `assertEqual` Just "-webkit-box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2); -moz-box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2); -ms-box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2); -o-box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2); box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2)"
  renderedInline multipleShadows `assertEqual` Just "-webkit-box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%); -moz-box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%); -ms-box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%); -o-box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%); box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%)"

  renderedInline exampleDirection `assertEqual` Just "direction: rtl"
