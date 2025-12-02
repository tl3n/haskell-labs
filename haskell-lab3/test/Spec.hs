module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Data.List (sort, nub)

import TextProcessor

-- ==========================================
-- Tests for normalizeWhitespace
-- ==========================================

testNormalizeWhitespace :: Test
testNormalizeWhitespace = TestLabel "normalizeWhitespace" $ TestList
    [ TestCase $ assertEqual "single spaces unchanged" 
        "hello world" 
        (normalizeWhitespace "hello world")
    
    , TestCase $ assertEqual "multiple spaces to single" 
        "hello world" 
        (normalizeWhitespace "hello    world")
    
    , TestCase $ assertEqual "tabs to spaces" 
        "hello world" 
        (normalizeWhitespace "hello\tworld")
    
    , TestCase $ assertEqual "newlines to spaces" 
        "hello world" 
        (normalizeWhitespace "hello\nworld")
    
    , TestCase $ assertEqual "mixed whitespace" 
        "hello world test" 
        (normalizeWhitespace "hello  \t  world\n\ntest")
    
    , TestCase $ assertEqual "leading and trailing whitespace removed" 
        "hello world" 
        (normalizeWhitespace "   hello world   ")
    
    , TestCase $ assertEqual "empty string" 
        "" 
        (normalizeWhitespace "")
    
    , TestCase $ assertEqual "only whitespace" 
        "" 
        (normalizeWhitespace "     ")
    ]

-- ==========================================
-- Tests for isValidChar
-- ==========================================

testIsValidChar :: Test
testIsValidChar = TestLabel "isValidChar" $ TestList
    [ TestCase $ assertEqual "lowercase letter" True (isValidChar 'a')
    , TestCase $ assertEqual "uppercase letter" True (isValidChar 'Z')
    , TestCase $ assertEqual "hyphen" True (isValidChar '-')
    , TestCase $ assertEqual "apostrophe" True (isValidChar '\'')
    , TestCase $ assertEqual "digit not valid" False (isValidChar '5')
    , TestCase $ assertEqual "space not valid" False (isValidChar ' ')
    , TestCase $ assertEqual "punctuation not valid" False (isValidChar '.')
    , TestCase $ assertEqual "comma not valid" False (isValidChar ',')
    , TestCase $ assertEqual "cyrillic letter" True (isValidChar 'ф')
    , TestCase $ assertEqual "cyrillic uppercase" True (isValidChar 'Ї')
    ]

-- ==========================================
-- Tests for cleanWord
-- ==========================================

testCleanWord :: Test
testCleanWord = TestLabel "cleanWord" $ TestList
    [ TestCase $ assertEqual "plain word unchanged" 
        "hello" 
        (cleanWord "hello")
    
    , TestCase $ assertEqual "remove trailing punctuation" 
        "hello" 
        (cleanWord "hello!")
    
    , TestCase $ assertEqual "remove leading punctuation" 
        "hello" 
        (cleanWord "\"hello")
    
    , TestCase $ assertEqual "remove surrounding punctuation" 
        "hello" 
        (cleanWord "(hello)")
    
    , TestCase $ assertEqual "remove numbers" 
        "hello" 
        (cleanWord "hello123")
    
    , TestCase $ assertEqual "keep only letters" 
        "helloworld" 
        (cleanWord "hello-world")  -- Note: hyphen is removed by cleanWord
    
    , TestCase $ assertEqual "empty when only punctuation" 
        "" 
        (cleanWord "...!!!")
    
    , TestCase $ assertEqual "mixed content" 
        "test" 
        (cleanWord "123test456")
    
    , TestCase $ assertEqual "cyrillic word" 
        "привіт" 
        (cleanWord "привіт!")
    ]

-- ==========================================
-- Tests for extractWords
-- ==========================================

testExtractWords :: Test
testExtractWords = TestLabel "extractWords" $ TestList
    [ TestCase $ assertEqual "simple sentence" 
        ["hello", "world"] 
        (extractWords "Hello World")
    
    , TestCase $ assertEqual "with punctuation" 
        ["hello", "world"] 
        (extractWords "Hello, World!")
    
    , TestCase $ assertEqual "multiple spaces" 
        ["hello", "world"] 
        (extractWords "Hello    World")
    
    , TestCase $ assertEqual "all lowercase result" 
        ["test", "case"] 
        (extractWords "TEST CASE")
    
    , TestCase $ assertEqual "with numbers" 
        ["year"] 
        (extractWords "2024 year")
    
    , TestCase $ assertEqual "empty string" 
        [] 
        (extractWords "")
    
    , TestCase $ assertEqual "only punctuation" 
        [] 
        (extractWords "... !!! ???")
    
    , TestCase $ assertEqual "complex sentence" 
        ["the", "quick", "brown", "fox", "jumps"] 
        (extractWords "The quick, brown fox jumps!")
    
    , TestCase $ assertEqual "words with hyphens become merged" 
        ["wellknown"] 
        (extractWords "well-known")
    
    , TestCase $ assertEqual "quoted words" 
        ["hello"] 
        (extractWords "\"hello\"")
    
    , TestCase $ assertEqual "newlines handled" 
        ["line", "one", "line", "two"] 
        (extractWords "line one\nline two")
    ]

-- ==========================================
-- Tests for groupByFirstLetter
-- ==========================================

testGroupByFirstLetter :: Test
testGroupByFirstLetter = TestLabel "groupByFirstLetter" $ TestList
    [ TestCase $ assertEqual "already sorted by letter" 
        [["apple", "ant"], ["banana", "berry"]] 
        (groupByFirstLetter ["apple", "ant", "banana", "berry"])
    
    , TestCase $ assertEqual "single group" 
        [["apple", "ant", "avocado"]] 
        (groupByFirstLetter ["apple", "ant", "avocado"])
    
    , TestCase $ assertEqual "each word separate group" 
        [["apple"], ["banana"], ["cherry"]] 
        (groupByFirstLetter ["apple", "banana", "cherry"])
    
    , TestCase $ assertEqual "empty list" 
        [] 
        (groupByFirstLetter [])
    
    , TestCase $ assertEqual "single word" 
        [["hello"]] 
        (groupByFirstLetter ["hello"])
    
    , TestCase $ assertEqual "mixed groups" 
        [["cat"], ["dog", "deer"], ["elephant"]] 
        (groupByFirstLetter ["cat", "dog", "deer", "elephant"])
    ]

-- ==========================================
-- Integration Tests
-- ==========================================

-- Note: groupByFirstLetter only groups CONSECUTIVE words with the same first letter.
-- In the main program, words are sorted first (with sort $ nub) before grouping.

testIntegration :: Test
testIntegration = TestLabel "Integration" $ TestList
    [ TestCase $ assertEqual "full pipeline with sort (like main program)" 
        [["fox"], ["lazy"], ["over"], ["quick"], ["the"]]
        (groupByFirstLetter $ sort $ nub $ extractWords "The quick fox. Over the lazy over.")
    
    , TestCase $ assertEqual "extractWords then groupByFirstLetter (consecutive grouping only)" 
        [["the"], ["quick"], ["fox"], ["over"], ["the"], ["lazy"], ["over"]]
        (groupByFirstLetter $ extractWords "The quick fox. Over the lazy over.")
    
    , TestCase $ assertEqual "text with various punctuation" 
        [["hello", "hi"], ["world"]]
        (groupByFirstLetter $ extractWords "Hello, hi! World.")
    
    , TestCase $ assertEqual "consecutive same-letter words grouped" 
        [["a", "a", "a"]]
        (groupByFirstLetter $ extractWords "a A a")
    ]

-- ==========================================
-- Edge Case Tests
-- ==========================================

testEdgeCases :: Test
testEdgeCases = TestLabel "Edge Cases" $ TestList
    [ TestCase $ assertEqual "unicode text" 
        ["привіт", "світ"]
        (extractWords "Привіт, світ!")
    
    , TestCase $ assertEqual "very long word" 
        ["supercalifragilisticexpialidocious"]
        (extractWords "supercalifragilisticexpialidocious")
    
    , TestCase $ assertEqual "single character words" 
        ["a", "b", "c"]
        (extractWords "a b c")
    
    , TestCase $ assertEqual "tabs and newlines mixed" 
        ["one", "two", "three"]
        (extractWords "one\ttwo\nthree")
    ]

-- ==========================================
-- Main test runner
-- ==========================================

allTests :: Test
allTests = TestList
    [ testNormalizeWhitespace
    , testIsValidChar
    , testCleanWord
    , testExtractWords
    , testGroupByFirstLetter
    , testIntegration
    , testEdgeCases
    ]

main :: IO ()
main = do
    results <- runTestTT allTests
    if errors results + failures results == 0
        then exitSuccess
        else exitFailure