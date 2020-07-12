-- From this stackoverflow answer: https://emacs.stackexchange.com/a/54408
-- The purpose is to remove extraneous "PROPERTIES"
import Text.Pandoc.JSON

main = toJSONFilter noAttrs

noAttrs :: Block -> Block
noAttrs (Header n _ i) = Header n nullAttr i
noAttrs (Div _ b) = Div nullAttr b
noAttrs b = b
