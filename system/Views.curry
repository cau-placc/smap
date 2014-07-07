--------------------------------------------------------------------------------
--- This module provides auxiliary functionalities for the general use in views
--- such as confirmation dialog and WUI form construction. This module should be
--- imported by all controller modules, given that it also exports the general
--- type synonym for views.
---
--- @author Lasse Kristopher Meyer
--- @version March 2014
--------------------------------------------------------------------------------

module Views (
  View,wuiWithErrorFormToHtml,wuiFrameToForm,
  renderWuiForm,withConfirmation
) where

import Prelude hiding (div)

import Controllers
import SmapHtml
import SmapWui

--------------------------------------------------------------------------------

--- A view is a list of HTML expressions that will be framed by the general
--- layout of the HTML page (see `getForm` in `Controllers`) and contains the
--- viewable content of a HTML page.
type View = [HtmlExp]

--------------------------------------------------------------------------------
-- Helpful HTML components for generic views                                  --
--------------------------------------------------------------------------------

--- Provides a confirmation dialog box for a given trigger element and a HTML
--- handler that handles the confirmable action. 
--- @param dialogId - the ID of the confirmation dialog - if multiple dialogs
---   are used in one view each of it must get a unique id
--- @param trigger - the element that will trigger the dialog box
--- @param message - the confirmation message shown in the dialog box
--- @param hdlr    - the HTML handler that fires the confirmed action
--- @return a pair containing the given trigger element with its actual
---   functionality added and the confirmation dialog box as a HTML expression;
---   the first component should be used in place of the given trigger element
---   and the second component must be inserted somewhere in the HTML form to
---   actually show up when the returned trigger element is clicked
withConfirmation :: Int -> HtmlExp -> String -> HtmlHandler -> (HtmlExp,HtmlExp)
withConfirmation dialogId trigger message hdlr =
  (trigger `addAttrs` [modalToggle,targetId modalId]
  ,stdModal modalId labelId
    [glyphicon "question-sign",text " Do you really want to proceed?"]
    [text message]
    [buttonButton [class "btn btn-default",modalDismiss]
      [text "Cancel"]
    ,submitButton [class "btn btn-success"] hdlr 
      [glyphicon "ok",text " Confirm"]]
  `addToClass` "confirmation-dialog-box")
  where 
    modalId = "confirmation-dialog-box-"++show dialogId
    labelId = modalId++"-title"

--------------------------------------------------------------------------------
-- General WUI forms and WUI components                                       --
--------------------------------------------------------------------------------

--- Standard rendering for WUI forms. Defines holes for a header, an info line,
--- the submit button label, additional navigation elements and a panel footer.
--- Also takes initial data to be prefilled in the form and the controller that
--- handles the form submission.
--- @param wuiSpec - the associated WUI specification
--- @param init    - initial data to be prefilled in the form
--- @param ctrl    - the controller that handles the for submission
--- @param header  - header HRML expressions
--- @param info    - info line HTML expressions
--- @param label   - submit button label HTML expressions
--- @param addNav  - additional navigation elements (placed right to the submit
---   button)
--- @param footer  - footer HTML expressions
renderWuiForm
  :: WuiSpec a -> a -> (a -> Controller) -> [HtmlExp] -> [HtmlExp] -> [HtmlExp]
  -> [HtmlExp] -> [HtmlExp] -> [HtmlExp]
renderWuiForm wuiSpec init ctrl header info label addNav footer =
  wuiFrame hExp wHdlr
  where
    (hExp,wHdlr) = wuiWithErrorFormToHtml wuiSpec 
                                          init
                                          ctrl
                                          (wuiFrameToForm wuiFrame)
    wuiFrame hExp' wHdlr' =
      [(container `withId` "wui-form")
        [row
          [col [Xs 6,XsOffset 3]
            [panelDefault
              [panelBody $
                [(pageHeader `withId` "header")
                  header
                ,mInfo
                ,hExp']
              ++[div [class "pull-left"]
                  [blueWuiBtn wHdlr' label,greyCancelBtn]
                ,div [class "pull-right"]
                  addNav]
          ,panelFooter
            footer]]]]]
    mInfo = if null info then empty else div [class "info"] info

--- Adapts the `wuiWithErrorForm` function to Spicey's controller concept.
--- @param wuiSpec    - the associated WUI specification
--- @param initial    - initial data to be prefilled in the form
--- @param controller - the controller that handles the submission
--- @param errorForm  - the form for the error case
wuiWithErrorFormToHtml
  :: WuiSpec a -> a -> (a -> Controller) -> (HtmlExp -> WuiHandler 
  -> IO HtmlForm) -> (HtmlExp,WuiHandler)
wuiWithErrorFormToHtml wuiSpec initial controller errorForm =
  wuiWithErrorForm wuiSpec initial (nextFor controller) errorForm

--- Builds the HTML form from a given WUI form.
--- @param wuiFrame - the frame for the WUI form
--- @param hexp     - HTML expressions specifying the WUI form
--- @param whdlr    - the associated WUI handler
wuiFrameToForm
  :: (HtmlExp -> WuiHandler -> [HtmlExp]) -> HtmlExp -> WuiHandler
  -> IO HtmlForm
wuiFrameToForm wuiFrame hexp whdlr = getForm (wuiFrame hexp whdlr)

--------------------------------------------------------------------------------