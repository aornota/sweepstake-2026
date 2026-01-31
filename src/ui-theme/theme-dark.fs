module Aornota.Sweepstake2026.Ui.Theme.Dark

open Aornota.Sweepstake2026.Ui.Theme.Common

let private transformSemantic semantic = match semantic with | Black -> White | Dark -> Light | Light -> Dark | White -> Black | _ -> semantic

let private transformSemanticOption semantic = match semantic with | Some semantic -> Some (transformSemantic semantic) | None -> None

let private transformParaColour paraColour =
    let transformGreyscale greyscale =
        match greyscale with
        | BlackBis -> WhiteBis | BlackTer -> WhiteTer | GreyDarker -> GreyLighter | GreyDark -> GreyLight
        | Grey -> Grey
        | GreyLight -> GreyDark | GreyLighter -> GreyDarker | WhiteTer -> BlackTer | WhiteBis -> BlackBis
    match paraColour with
    | DefaultPara -> DefaultPara
    | SemanticPara semantic -> SemanticPara (transformSemantic semantic)
    | GreyscalePara greyscale -> GreyscalePara (transformGreyscale greyscale)

let private transformSpanClassOption spanClass =
    let transformSpanClass spanClass = match spanClass with | Healthy -> Unhealthy | Unhealthy -> Healthy
    match spanClass with | Some spanClass -> Some (transformSpanClass spanClass) | None -> None

let themeDark = {
    ThemeClass = ThemeClass "dark"
    AlternativeClass = AlternativeClass "dark-alternative"
    TransformButtonData = (fun buttonData -> { buttonData with ButtonSemantic = transformSemanticOption buttonData.ButtonSemantic })
    TransformMessageData = (fun messageData -> { messageData with MessageSemantic = transformSemanticOption messageData.MessageSemantic })
    TransformNavbarData = (fun navbarData -> { navbarData with NavbarSemantic = transformSemanticOption navbarData.NavbarSemantic })
    TransformNotificationData = (fun notificationData -> { notificationData with NotificationSemantic = transformSemanticOption notificationData.NotificationSemantic })
    TransformPageLoaderData = (fun pageLoaderData -> { pageLoaderData with PageLoaderSemantic = transformSemantic pageLoaderData.PageLoaderSemantic })
    TransformParaData = (fun paraData -> { paraData with ParaColour = transformParaColour paraData.ParaColour })
    TransformProgressData = (fun progressData -> { progressData with ProgressSemantic = transformSemanticOption progressData.ProgressSemantic })
    TransformRadioData = (fun radioData -> { radioData with RadioSemantic = transformSemanticOption radioData.RadioSemantic })
    TransformSpanData = (fun spanData -> { spanData with SpanClass = transformSpanClassOption spanData.SpanClass })
    TransformTableData = id
    TransformTabsData = id
    TransformTagData = (fun tagData -> { tagData with TagSemantic = transformSemanticOption tagData.TagSemantic })
    TransformTooltipData = (fun tooltipData -> { tooltipData with TooltipSemantic = transformSemanticOption tooltipData.TooltipSemantic }) }
