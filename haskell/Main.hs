module Main (main) where

import Control.Exception (assert)
import Data.Char (ord)
import Data.Map (Map, insert, empty, fromList)

data JsonNode
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonNode]
  | JsonObject (Map String JsonNode)

instance Eq JsonNode where
  JsonNull == JsonNull = True
  JsonBool x == JsonBool y = x == y
  JsonNumber x == JsonNumber y = x == y
  JsonString x == JsonString y = x == y
  JsonArray x == JsonArray y = x == y
  JsonObject x == JsonObject y = x == y
  x == y = False

instance Show JsonNode where
  show n = case n of
    JsonNull -> "null"
    JsonBool b -> show b
    JsonNumber n -> show n
    JsonString s -> s
    JsonArray nodes -> show nodes
    JsonObject m -> show m

assertEq :: (Eq t, Show t) => (t, t) -> IO ()
assertEq (a, b)
  | a == b = putStrLn ("OK: " ++ show a)
  | otherwise = error ("ERR: " ++ show a ++ " != " ++ show b)

type Input = String
type Remainder = String

parseString :: (Input, String, Bool) -> (JsonNode, Remainder)
parseString (input, output, isReadingEscapedChar) =
  let (h : t) = input
   in if isReadingEscapedChar
        then parseString (t, output ++ [h], False)
        else case h of
          '\\' -> parseString (t, output, True)
          '"' -> (JsonString output, t)
          _ -> parseString (t, output ++ [h], False)

parseArray :: (Input, [JsonNode]) -> (JsonNode, Remainder)
parseArray (input, output) = case skipWhitespace input of
  (',' : t) -> parseArray (t, output)
  (']' : t) -> (JsonArray output, t)
  _ ->
    let (node, rem) = parseNext input
     in parseArray (rem, output ++ [node])

parseNumber :: (Input, String) -> (JsonNode, Remainder)
parseNumber (input, output) =
  let (h : t) = input
   in if isNumberLike h
        then
          parseNumber (t, output ++ [h])
        else
          (JsonNumber (read output :: Double), input)

expect :: (Char, String) -> String
expect (ch, s) =
  let (h : t) = s in
  if ch == h then
    t
  else
    error ("expected '" ++ [ch] ++ "', found '" ++ [h] ++ "'")

parseObject :: (Input, Map String JsonNode) -> (JsonNode, Remainder)
parseObject (input, output) =
  let s = skipWhitespace input in
  case s of
    ('}' : t) -> (JsonObject output, t)
    (',' : t) -> parseObject(t, output)
    _ ->
      let x1 = expect ('"', s) in
      let (JsonString key, x2) = parseString (x1, "", False) in
      let x3 = expect (':', skipWhitespace x2) in
      let (node, x4) = parseNext x3 in
      parseObject (x4, insert key node output)

parseNext :: Input -> (JsonNode, Remainder)
parseNext p =
  let s = skipWhitespace p
   in if isNumberLike (head s)
        then
          parseNumber (s, "")
        else case s of
          ('n' : 'u' : 'l' : 'l' : t) -> (JsonNull, t)
          ('t' : 'r' : 'u' : 'e' : t) -> (JsonBool True, t)
          ('f' : 'a' : 'l' : 's' : 'e' : t) -> (JsonBool False, t)
          ('"' : t) -> parseString (t, "", False)
          ('[' : t) -> parseArray (t, [])
          ('{' : t) -> parseObject (t, empty)

isWhitespace :: Char -> Bool
isWhitespace ch = ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t'

isNumberLike :: Char -> Bool
isNumberLike ch =
  ch `elem` ['0' .. '9'] || ch == '.'

skipWhitespace :: Input -> String
skipWhitespace p
  | null p = ""
  | isWhitespace (head p) = skipWhitespace (tail p)
  | otherwise = p

parse :: Input -> JsonNode
parse s =
  let (n, x1) = parseNext s in
  let x2 = skipWhitespace x1 in
  if not (null x2) then
    error ("expected EOF, received \"" ++ x2 ++ "\"")
  else
    n

runTests :: IO ()
runTests = do
  putStrLn "Running tests:"

  let rem = expect ('"', "\"yee")
  assertEq ("yee", rem)

  assertEq (True, isNumberLike '0')
  assertEq (True, isNumberLike '9')
  assertEq (True, isNumberLike '.')
  assertEq (False, isNumberLike 'f')
  assertEq ("yee!", skipWhitespace "  yee!")

  -- parse next
  let n = parse "null"
  assertEq (JsonNull, n)

  -- expects & removes double quotes
  let JsonString s = parse "\"Hello World\""
  assertEq ("Hello World", s)

  -- reads quoted char
  let JsonString s = parse "\"hello\\\"\""
  assertEq ("hello\"", s)

  -- array
  let JsonArray n = parse "[null, null, 123.5]"
  assertEq ([JsonNull, JsonNull, JsonNumber 123.5], n)

  -- array with whitespace
  let JsonArray n = parse "[ 1 , 2 , 3 ]"
  assertEq ([JsonNumber 1, JsonNumber 2, JsonNumber 3], n)

  -- empty object
  let JsonObject n = parse "{}"
  assertEq (empty, n)

  -- object with nested stuff
  let JsonObject n = parse "{ \"a\" : 123, \"b\" : [] }"
  assertEq (fromList [("a", JsonNumber 123), ("b", JsonArray [])], n)

main :: IO ()
main = do
  runTests
