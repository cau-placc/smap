--------------------------------------------------------------------------------
--- This module provides auxiliary functionalities for the general use in views
--- such as confirmation dialog and WUI form construction. This module should be
--- imported by all controller modules, given that it also exports the general
--- type synonym for views.
---
--- @author Lasse Kristopher Meyer, Michael Hanus
--- @version October 2020
--------------------------------------------------------------------------------

module System.Views (
  View, withConfirmation, renderWui
) where

import Prelude hiding (div)

import System.Controllers
import System.SmapHtml
import System.SmapWui

--------------------------------------------------------------------------------

--- A view is a list of HTML expressions that will be framed by the general
--- layout of the HTML page (see `getPage` in `Controllers`) and contains the
--- viewable content of a HTML page.
type View = [BaseHtml]

--------------------------------------------------------------------------------
-- Helpful HTML components for generic views                                  --
--------------------------------------------------------------------------------

--- Provides a confirmation dialog box for a given trigger element and a HTML
--- handler that handles the confirmable action. 
--- @param dialogId - the ID of the confirmation dialog - if multiple dialogs
---   are used in one view each of it must get a unique id
--- @param trigger - the element that will trigger the dialog box
--- @param message - the confirmation message shown in the dialog box
--- @param acturl  - the URL of the confirmed action
--- @return a pair containing the given trigger element with its actual
---   functionality added and the confirmation dialog box as a HTML expression;
---   the first component should be used in place of the given trigger element
---   and the second component must be inserted somewhere in the HTML form to
---   actually show up when the returned trigger element is clicked
withConfirmation :: HTML h => Int -> h -> String -> String -> (h,h)
withConfirmation dialogId trigger message acturl =
  (trigger `addAttrs` [modalToggle,targetId modalId]
  ,stdModal modalId labelId
    [glyphicon "question-sign",text " Do you really want to proceed?"]
    [text message]
    [buttonButton [classA "btn btn-default",modalDismiss]
      [text "Cancel"]
    ,greenLinkBtn acturl [glyphicon "ok", htxt "Confirm"]
    ]
  `addToClass` "confirmation-dialog-box")
  where 
    modalId = "confirmation-dialog-box-" ++ show dialogId
    labelId = modalId ++ "-title"

--------------------------------------------------------------------------------
-- General WUI forms and WUI components                                       --
--------------------------------------------------------------------------------

--- Standard rendering for WUI forms.
--- It has parameters for a header, an info line,
--- the submit button label, additional navigation elements and a panel footer.
--- @param header  - header HTML expressions
--- @param info    - info line HTML expressions
--- @param label   - label for submit button
--- @param addNav  - additional navigation elements (placed right to the submit
---   button)
--- @param footer  - footer HTML expressions
--- @param hexp    - the HTML expression representing the WUI form
--- @param handler - the handler for submitting data
renderWui :: [HtmlExp] -> [HtmlExp] -> String -> [HtmlExp] -> [HtmlExp]
          -> HtmlExp -> (HtmlEnv -> IO [BaseHtml]) -> [HtmlExp]
renderWui header info label addNav footer hexp hdlr =
      [(container `withId` "wui-form")
        [row
          [col [Xs 6, XsOffset 3]
            [panelDefault
              [panelBody $
                [(pageHeader `withId` "header")
                  header
                ,mInfo
                ,hexp] ++
                [div [classA "pull-left"]
                  [ blueSubmitBtn label (\env -> hdlr env >>= getPage)
                  , greyCancelBtn]
                ,div [classA "pull-right"]
                  addNav]
          ,panelFooter
            footer]]]]]
 where
  mInfo = if null info then empty else div [classA "info"] info

--------------------------------------------------------------------------------