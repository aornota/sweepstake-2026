module Aornota.Sweepstake2026.Ui.Theme.Common

open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common

open Browser.Types

type ThemeClass = | ThemeClass of themeClass : string

type AlternativeClass = | AlternativeClass of alternativeClass : string

type Semantic = | Primary | Info | Link | Success | Warning | Danger | Dark | Light | Black | White

type Greyscale = | BlackBis | BlackTer | GreyDarker | GreyDark | Grey | GreyLight | GreyLighter | WhiteTer | WhiteBis

type TooltipPosition = | TooltipTop | TooltipRight | TooltipBottom | TooltipLeft

type TooltipData = {
    TooltipSemantic : Semantic option
    Position : TooltipPosition
    IsMultiLine : bool
    TooltipText : string }

type ButtonInteraction =
    | Clickable of onClick : (MouseEvent -> unit) * tooltipData : TooltipData option
    | Loading
    | Static
    | NotEnabled of tooltipData : TooltipData option

type ButtonData = {
    ButtonSemantic : Semantic option
    ButtonSize : Size
    IsOutlined : bool
    IsInverted : bool
    IsText : bool
    Interaction : ButtonInteraction
    IconLeft : IconData option
    IconRight : IconData option }

type LinkType =
    | Internal of onClick : (MouseEvent -> unit)
    | NewWindow of url : string
    | DownloadFile of url : string * fileName : string

type MessageData = {
    MessageSemantic : Semantic option
    MessageSize : Size
    OnDismissMessage : (MouseEvent -> unit) option }

type NavbarFixed = | FixedTop | FixedBottom

type NavbarData = {
    NavbarSemantic : Semantic option
    NavbarFixed : NavbarFixed option }

type NotificationData = {
    NotificationSemantic : Semantic option
    OnDismissNotification : (MouseEvent -> unit) option }

type PageLoaderData = { PageLoaderSemantic : Semantic }

type ParaColour =
    | DefaultPara
    | SemanticPara of semantic : Semantic
    | GreyscalePara of greyscale : Greyscale

type ParaSize = | LargestText | LargerText | LargeText | MediumText | SmallText | SmallerText | SmallestText

type ParaWeight = | LightWeight | NormalWeight | SemiBold | Bold

type ParaData = {
    ParaAlignment : Alignment
    ParaColour : ParaColour
    ParaSize : ParaSize
    Weight : ParaWeight }

type ProgressData = {
    ProgressSemantic : Semantic option
    ProgressSize : Size
    Value : int
    MaxValue : int }

type RadioData = {
    RadioSemantic : Semantic option
    RadioSize : Size
    HasBackgroundColour : bool }

type SpanClass = | Healthy | Unhealthy

type SpanData = { SpanClass : SpanClass option }

type TableData = {
    IsBordered : bool
    IsNarrow : bool
    IsStriped : bool
    IsFullWidth : bool }

type TabData = {
    IsActive : bool
    TabText : string
    TabLinkType : LinkType }

type TabsData = {
    IsBoxed : bool
    IsToggle : bool
    // TODO-NMB-LOW?... IsToggleRounded : bool
    TabsAlignment : Alignment
    TabsSize : Size
    Tabs : TabData list }

type TagData = {
    TagSemantic : Semantic option
    TagSize : Size
    IsRounded : bool
    OnDismiss : (MouseEvent -> unit) option }

type Theme = {
    ThemeClass : ThemeClass
    AlternativeClass : AlternativeClass
    TransformButtonData : ButtonData -> ButtonData
    TransformMessageData : MessageData -> MessageData
    TransformNavbarData : NavbarData -> NavbarData
    TransformNotificationData : NotificationData -> NotificationData
    TransformPageLoaderData : PageLoaderData -> PageLoaderData
    TransformParaData : ParaData -> ParaData
    TransformProgressData : ProgressData -> ProgressData
    TransformRadioData : RadioData -> RadioData
    TransformSpanData : SpanData -> SpanData
    TransformTableData : TableData -> TableData
    TransformTabsData : TabsData -> TabsData
    TransformTagData : TagData -> TagData
    TransformTooltipData : TooltipData -> TooltipData }

let [<Literal>] private EMPTY_STRING = ""

let getThemeClass (ThemeClass themeClass) = themeClass
let getAlternativeClass (AlternativeClass alternativeClass) = alternativeClass

let buttonDefault = {
    ButtonSemantic = None ; ButtonSize = Normal ; IsOutlined = false ; IsInverted = false ; IsText = false ; Interaction = NotEnabled None ; IconLeft = None ; IconRight = None }
let buttonDefaultSmall = { buttonDefault with ButtonSize = Small }
let buttonPrimary = { buttonDefault with ButtonSemantic = Some Primary }
let buttonPrimarySmall = { buttonPrimary with ButtonSize = Small }
let buttonInfo = { buttonDefault with ButtonSemantic = Some Info }
let buttonInfoSmall = { buttonInfo with ButtonSize = Small }
let buttonLink = { buttonDefault with ButtonSemantic = Some Link }
let buttonLinkSmall = { buttonLink with ButtonSize = Small }
let buttonSuccess = { buttonDefault with ButtonSemantic = Some Success }
let buttonSuccessSmall = { buttonSuccess with ButtonSize = Small }
let buttonWarning = { buttonDefault with ButtonSemantic = Some Warning }
let buttonWarningSmall = { buttonWarning with ButtonSize = Small }
let buttonDanger = { buttonDefault with ButtonSemantic = Some Danger }
let buttonDangerSmall = { buttonDanger with ButtonSize = Small }
let buttonDark = { buttonDefault with ButtonSemantic = Some Dark }
let buttonDarkSmall = { buttonDark with ButtonSize = Small }
let buttonLight = { buttonDefault with ButtonSemantic = Some Light }
let buttonLightSmall = { buttonLight with ButtonSize = Small }
let buttonBlack = { buttonDefault with ButtonSemantic = Some Black }
let buttonBlackSmall = { buttonBlack with ButtonSize = Small }
let buttonWhite = { buttonDefault with ButtonSemantic = Some White }
let buttonWhiteSmall = { buttonWhite with ButtonSize = Small }

let messageDefault = { MessageSemantic = None ; MessageSize = Normal ; OnDismissMessage = None }
let messagePrimary = { messageDefault with MessageSemantic = Some Primary }
let messageInfo = { messageDefault with MessageSemantic = Some Info }
let messageLink = { messageDefault with MessageSemantic = Some Link }
let messageSuccess = { messageDefault with MessageSemantic = Some Success }
let messageWarning = { messageDefault with MessageSemantic = Some Warning }
let messageDanger = { messageDefault with MessageSemantic = Some Danger }
let messageDark = { messageDefault with MessageSemantic = Some Dark }
let messageLight = { messageDefault with MessageSemantic = Some Light }
let messageBlack = { messageDefault with MessageSemantic = Some Black }
let messageWhite = { messageDefault with MessageSemantic = Some White }

let navbarDefault = { NavbarSemantic = None ; NavbarFixed = Some FixedTop }

let notificationDefault = { NotificationSemantic = None ; OnDismissNotification = None }
let notificationPrimary = { notificationDefault with NotificationSemantic = Some Primary}
let notificationInfo = { notificationDefault with NotificationSemantic = Some Info}
let notificationLink = { notificationDefault with NotificationSemantic = Some Link}
let notificationSuccess = { notificationDefault with NotificationSemantic = Some Success}
let notificationWarning = { notificationDefault with NotificationSemantic = Some Warning}
let notificationDanger = { notificationDefault with NotificationSemantic = Some Danger}
let notificationDark = { notificationDefault with NotificationSemantic = Some Dark}
let notificationLight = { notificationDefault with NotificationSemantic = Some Light}
let notificationBlack = { notificationDefault with NotificationSemantic = Some Black}
let notificationWhite = { notificationDefault with NotificationSemantic = Some White}

let pageLoaderDefault = { PageLoaderSemantic = Light }

let paraDefaultSmallest = { ParaAlignment = LeftAligned ; ParaColour = DefaultPara ; ParaSize = SmallestText ; Weight = NormalWeight }
let paraDefaultSmaller = { paraDefaultSmallest with ParaSize = SmallerText }
let paraDefaultSmall = { paraDefaultSmallest with ParaSize = SmallText }
let paraDefaultMedium = { paraDefaultSmallest with ParaSize = MediumText }
let paraDefaultLarge = { paraDefaultSmallest with ParaSize = LargeText }
let paraDefaultLarger = { paraDefaultSmallest with ParaSize = LargerText }
let paraDefaultLargest = { paraDefaultSmallest with ParaSize = LargestText }
let paraCentredSmallest = { paraDefaultSmallest with ParaAlignment = Centred }
let paraCentredSmaller = { paraCentredSmallest with ParaSize = SmallerText }
let paraCentredSmall = { paraCentredSmallest with ParaSize = SmallText }
let paraCentredMedium = { paraCentredSmallest with ParaSize = MediumText }
let paraCentredLarge = { paraCentredSmallest with ParaSize = LargeText }
let paraCentredLarger = { paraCentredSmallest with ParaSize = LargerText }
let paraCentredLargest = { paraCentredSmallest with ParaSize = LargestText }

let progressDefault = { ProgressSemantic = None ; ProgressSize = Normal ; Value = 0 ; MaxValue = 100 }

let radioDefault = { RadioSemantic = None ; RadioSize = Normal ; HasBackgroundColour = false }
let radioDefaultSmall = { radioDefault with RadioSize = Small }

let spanDefault = { SpanClass = None }

let tableDefault = { IsBordered = false ; IsNarrow = false ; IsStriped = false ; IsFullWidth = false }
let tableFullWidth = { tableDefault with IsFullWidth = true }
let tableStriped = { tableDefault with IsStriped = true }
let tableFullWidthStriped = { tableFullWidth with IsStriped = true }

let tabsDefault = { IsBoxed = false ; IsToggle = false ; TabsAlignment = LeftAligned ; TabsSize = Small ; Tabs = [] }

let tagDefault = { TagSemantic = None ; TagSize = Normal ; IsRounded = true ; OnDismiss = None }
let tagDefaultMedium = { tagDefault with TagSize = Medium }
let tagPrimary = { tagDefault with TagSemantic = Some Primary }
let tagPrimaryMedium = { tagPrimary with TagSize = Medium }
let tagInfo = { tagDefault with TagSemantic = Some Info }
let tagInfoMedium = { tagInfo with TagSize = Medium }
let tagLink = { tagDefault with TagSemantic = Some Link }
let tagLinkMedium = { tagLink with TagSize = Medium }
let tagSuccess = { tagDefault with TagSemantic = Some Success }
let tagSuccessMedium = { tagSuccess with TagSize = Medium }
let tagWarning = { tagDefault with TagSemantic = Some Warning }
let tagWarningMedium = { tagWarning with TagSize = Medium }
let tagDanger = { tagDefault with TagSemantic = Some Danger }
let tagDangerMedium = { tagDanger with TagSize = Medium }
let tagDark = { tagDefault with TagSemantic = Some Dark }
let tagDarkMedium = { tagDark with TagSize = Medium }
let tagLight = { tagDefault with TagSemantic = Some Light }
let tagLightMedium = { tagLight with TagSize = Medium }
let tagBlack = { tagDefault with TagSemantic = Some Black }
let tagBlackMedium = { tagBlack with TagSize = Medium }
let tagWhite = { tagDefault with TagSemantic = Some White }
let tagWhiteMedium = { tagWhite with TagSize = Medium }

let tooltipDefaultTop = { TooltipSemantic = Some Info ; Position = TooltipTop ; IsMultiLine = false ; TooltipText = EMPTY_STRING }
let tooltipDefaultRight = { tooltipDefaultTop with Position = TooltipRight }
let tooltipDefaultBottom = { tooltipDefaultTop with Position = TooltipBottom }
let tooltipDefaultLeft = { tooltipDefaultTop with Position = TooltipLeft }
