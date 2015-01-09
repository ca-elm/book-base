module Chapter where

import Graphics.Element
import List
import String
import Regex
import Maybe
import Slug (slugify)
import Theme (markdown)


type alias ChapterData =
  { title : String
  , content : String
  }

type alias Chapter =
  { title : String
  , slug : String
  , headings : List String
  , content : Graphics.Element.Element
  , number : Int
  }


makeChapters : List ChapterData -> List Chapter
makeChapters = List.indexedMap makeChapter

makeChapter : Int -> ChapterData -> Chapter
makeChapter i chapter =
  let slug = slugify chapter.title in
  { number = i + 1
  , title = chapter.title
  , slug = slug
  , content = markdown <| processMarkdown slug chapter.content
  , headings = List.map (String.dropLeft 2) <| List.filter ((==) "#" << String.left 1) <| String.split "\n" chapter.content
  }


processMarkdown : String -> String -> String
processMarkdown slug =
  Regex.replace Regex.All heading
  (\{submatches} ->
    let b = match submatches
        h = match <| List.tail submatches in
    b ++ "<h1 id=" ++ slug ++ "-" ++ slugify h ++ ">" ++ h ++ "</h1>")

match : List (Maybe String) -> String
match = Maybe.withDefault "" << List.head

heading : Regex.Regex
heading = Regex.regex "(^|[\n\r])#[ \\t]*(.+)"
