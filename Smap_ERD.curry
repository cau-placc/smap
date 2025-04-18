import Database.ERD

smapERD :: ERD
smapERD =
 ERD "Smap"

  [Entity "Metadata" 
    [Attribute "Title"       (StringDom Nothing     ) NoKey  False
    ,Attribute "Description" (StringDom Nothing     ) NoKey  True 
    ,Attribute "IsVisible"   (BoolDom   Nothing     ) NoKey  False]

  ,Entity "Version"
    [Attribute "Number"      (IntDom    Nothing     ) NoKey  False 
    ,Attribute "SourceCode"  (StringDom Nothing     ) NoKey  False
    ,Attribute "Message"     (StringDom Nothing     ) NoKey  False
    ,Attribute "Date"        (DateDom   Nothing     ) NoKey  False]

  ,Entity "Language"
    [Attribute "Name"        (StringDom Nothing     ) PKey   False
    ,Attribute "FilenameExt" (StringDom Nothing     ) Unique False
    ,Attribute "Template"    (StringDom Nothing     ) NoKey  True ]

  ,Entity "System"
    [Attribute "Name"        (StringDom Nothing     ) PKey   False
    ,Attribute "ExecUrl"     (StringDom Nothing     ) Unique False]

  ,Entity "User"
    [Attribute "Name"        (StringDom Nothing     ) PKey   False
    ,Attribute "Email"       (StringDom Nothing     ) Unique False
    ,Attribute "Hash"        (StringDom Nothing     ) NoKey  False
    ,Attribute "IsAdmin"     (BoolDom   (Just False)) NoKey  False]

  ,Entity "Tag"
    [Attribute "Name"        (StringDom Nothing     ) PKey   False]

  ,Entity "Comment"
    [Attribute "Text"        (StringDom Nothing     ) NoKey  False
    ,Attribute "Date"        (DateDom   Nothing     ) NoKey  False]]

  [Relationship "ImplLang"
    [REnd "Metadata" "isTheImplLangOf" (Between 0 Infinite)
    ,REnd "Language" "hasImplLang"     (Exactly 1)]

  ,Relationship "Authoring"
    [REnd "Metadata" "isTheAuthorOf"   (Between 0 Infinite)
    ,REnd "User"     "hasAuthor"       (Exactly 1)]

  ,Relationship "Versioning"
    [REnd "Metadata" "isAVersionOf"    (Exactly 1)
    ,REnd "Version"  "hasVersion"      (Between 1 Infinite)]

  ,Relationship "Tagging"
    [REnd "Metadata" "isATagOf"        (Between 0 Infinite)
    ,REnd "Tag"      "hasTag"          (Between 0 Infinite)]

  ,Relationship "CAuthoring"
    [REnd "User"     "hasCAuthor"      (Exactly 1)
    ,REnd "Comment"  "isTheCAuthorOf"  (Between 0 Infinite)]

  ,Relationship "Commenting"
    [REnd "Metadata" "isACommentOf"    (Exactly 1)
    ,REnd "Comment"  "hasComment"      (Between 0 Infinite)]

  ,Relationship "Favoriting"
    [REnd "User"     "isAFavoriteOf"   (Between 0 Infinite)
    ,REnd "Metadata" "hasFavorite"     (Between 0 Infinite)]

  ,Relationship "LangImpl"
    [REnd "Language" "isALangImplOf"   (Exactly 1)
    ,REnd "System"   "hasLangImpl"     (Between 1 Infinite)]]
