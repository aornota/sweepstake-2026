module Aornota.Sweepstake2026.Ui.Program.Markdown.Literals

let [<Literal>] SCORING_SYSTEM_MARKDOWN = """Each sweepstake team will consist of a **team/coach**, **1 goalkeeper** and **10 outfield players**.

(Although outfield players have been categorized as defenders, midfielders or forwards, you can choose any combination you like, e.g. if you want to go for eight defenders and two
midfielders - the no-longer-fashionable [except in Northern Ireland] 8-2-0 formation - please do.)

The **team/coach** will score (or lose) points for:
+ **winning** a match: **20** or **16** or **12** (see below)
+ **drawing** a match: **8** or **6** or **4** (see below)
+ a team player receiving a **yellow card**: **-1**
+ a team player receiving a **red card**: **-3**

(If a player receives a second yellow card in a match, the two yellow cards will be scored as a red card instead; however, if a player receives a yellow card followed by a "straight"
red card, both cards will be scored.)

Where multiple possible scores are given above, the score will depend on whether the team and their opponents are in the top 24 seeds:
+ if the team **is** a top 24 seed but their opponents are **not**, the **lowest** score will apply
+ if the team is **not** a top 24 seed but their opponents **are**, the **highest** score will apply
+ if **both** teams are top 24 seeds - or if **neither** team is - the **middle** score will apply

The top 24 seeds are (in order): Spain; Argentina; France; England; Brazil; Portugal; Netherlands; Belgium; Germany; Croatia; Morocco; Colombia; United States; Mexico; Uruguay; Switzerland;
Japan; Senegal; Iran; South Korea; Ecuador; Austria; Australia; and Canada.

(Note that these seedings are based on those detailed [here](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_seeding),)

The remaining teams are: Algeria; Bosnia and Herzegovina; Cape Verde; Czech Republic; Curaçao; DR Congo; Egypt; Ghana; Haiti; Iraq; Ivory Coast; Jordan; New Zealand; Norway; Panama; Paraguay;
Qatar; Saudi Arabia; Scotland; South Africa; Sweden; Tunisia; Turkey; and Uzbekistan.

**All players** will score (or lose) points for:
+ being named **man-of-the-match**: **10** if the player is a forward; **15** if the player is a midfielder; **20** if the player is a defender or a goalkeeper
+ **scoring** a **goal** or a **penalty**: **9** if the player is a forward; **12** if the player is a midfielder; **15** if the player is a defender; **20** if the player is a goalkeeper
+ **assisting** a **goal**: **3** (note that a goal cannot be assisted by the same player who scored the goal)
+ **missing** a **penalty**: **-6**
+ **scoring** an **own goal**: **-6**
+ receiving a **yellow card**: **-2**
+ receiving a **red card**: **-6**

(A penalty will be considered as "missed" irrespective of whether the goalkeeper touched the ball. And again, if a player receives a second yellow card in a match, the two yellow cards
will be scored as a red card instead; however, if a player receives a yellow card followed by a "straight" red card, both cards will be scored.)

In addition, **goalkeepers** will score points for:
+ keeping a **clean sheet**: **12**
+ **saving** a **penalty**: **12**

Note that outfield players can also score "goalkeeper" points if they end up playing in goal. (It probably won't happen - but you never know...)

(If more than one goalkeeper features for a team in a match, the "clean sheet" points will be awarded to whichever goalkeeper played more "regulation" minutes; if they played the same
amount of minutes, the points will be shared. A penalty will only be considered as "saved" if the goalkeeper touched the ball.)

Information about assists and such will be nicked from <https://www.whoscored.com/>.

As always, points can only be scored for goals / penalties / assists / &c. during normal time and extra time. **Penalty shootouts do not contribute to the scoring** [except to the extent
that they determine who wins the match] - well, unless a player manages to get booked or sent-off during the shootout. Stranger things have happened..."""

let [<Literal>] DRAFT_ALGORITHM_MARKDOWN = """This is not the easiest thing to explain - so let's try a simplified example:

**neph**, **rosie** and **hugh** submit the following selections for the first draft:

+ **neph**: _1._ Dante; _2._ Goethe; _3._ Saki
+ **rosie**: _1._ Cervantes; _2._ St. Augustine; _3._ Milton
+ **hugh**: _1._ Cervantes; _2._ Dante; _3._ St. Augustine

For the first round, we look at the top selection for each participant. Only **neph** has put Dante first and he gets an uncontested pick. However, both **rosie** and **hugh** fancy Cervantes, so we toss a (metaphorical) coin to resolve this contested pick: **rosie** wins on this occasion; **hugh** has his "pick priority" increased by way of compensation.

So after the first round: **neph** has picked Dante; **rosie** has picked Cervantes; **hugh** remains empty-handed.

Before the second round, we update each participant's list to remove teams / players that have now been picked. (As we're about to find out, this can have some slightly counter-intuitive consequences.)

The updated (and renumbered) selections are:

+ **neph**: _1._ Goethe; _2._ Saki
+ **rosie**: _1._ St. Augustine; _2._ Milton
+ **hugh**: _1._ St. Augustine

**neph** again has a unique selection for the second round and gets Goethe; **rosie** and **hugh** both want St. Augustine - and as **hugh** has the higher "pick priority" (having already lost one coin-toss), he wins automatically.

(Note that **hugh** ends up with St. Augustine even though this was third on his original list yet second on **rosie**'s. What can I say? Shit happens.)

After the second round: **neph** has picked Dante and Goethe; **rosie** has picked Cervantes; **hugh** has picked St. Augustine.

And for the third round, **neph** and **rosie** have uncontested picks (Saki and Milton respectively) and **hugh** has no selections left - so we end up with:

+ **neph** gets Dante, Goethe and Saki
+ **rosie** gets Cervantes and Milton
+ **hugh** gets St. Augustine

(Unfortunately for **neph**, Goethe gives Dante a "wet willy" in a group stage match and is sent home in disgrace; Dante is subsequently sidelined with a nasty ear infection; and Saki claims to have no interest whatsoever in playing football. **rosie**'s hopes are shattered when Cervantes misses a crucial penalty after a Zaza-esque run-up and Milton scores a hat-trick of own goals in the opening match. In the end, **hugh** emerges triumphant as St. Augustine wins the Golden Boot and cryptically claims: _And when by chance prosperity smiled in my direction, I lacked the spirit to seize it, for it fled away almost before I could get my hand upon it_. What a tosser.)

---
It's not a perfect algorithm by any means. But it's the best I've been able to come up with...

For a more detailed example, here are the [first and second draft details](http://aornota.github.io/sweepstake.2016/draft.html) for the world-famous Euro 2016 sweepstake."""

let [<Literal>] PAYOUTS_MARKDOWN = """##### **Payouts:**
+ **£TBC** for first place
+ **£TBC** for second place
+ **£TBC** for third place
+ **£TBC** for the cuchara de madera"""

let [<Literal>] MARKDOWN_SYNTAX_MARKDOWN = """# Markdown syntax
### A very quick introduction
Text can be:
+ **emboldened**
+ _italicized_
+ **_emboldened and italicized_**
+ ~~struck-through~~

This is a paragraph.
This is part of the same paragraph.

But this is a new paragraph.

This is a picture by the wonderful Gregory Kondos (RIP):

![Text if image not found...](https://tinyurl.com/y76sbjyr "Sacremento River with 32 Palms")

This is a list of Mdou Moctar albums:

| Name | Released |   |
|:-----|---------:|:-:|
| [_Afrique Victime_](https://mdoumoctar.bandcamp.com/album/afrique-victime) | May 2021 | ![](https://tinyurl.com/435x7vay) |
| [_Ilana: The Creator_](https://mdoumoctar.bandcamp.com/album/ilana-the-creator) | March 2019 | ![](https://tinyurl.com/y3285qgd "Like ZZ Top freaking out with Eddie Van Halen in 1975") |
| [_Blue Stage Session_](https://mdoumoctar.bandcamp.com/album/mdou-moctar-blue-stage-session) | January 2019 | ![](https://tinyurl.com/y6roz6yn "Live in Detroit") |
| [_Sousoume Tamachek_](https://mdoumoctar.bandcamp.com/album/sousoume-tamachek) | September 2017 | ![](https://tinyurl.com/ybjew7oo "Quite possibly my favourite album") |
| [_Akounak Tedalat Taha Tazoughai_](https://mdoumoctar.bandcamp.com/album/akounak-tedalat-taha-tazoughai-ost) (original soundtrack recording) | June 2015 | ![](https://tinyurl.com/y7hgyc77 "Soundtrack to a Tuareg language reimagining of 'Purple Rain'") |
| [_Anar_](https://mdoumoctar.bandcamp.com/album/anar) | September 2014 | ![](https://tinyurl.com/y7r3fby3) |
| [_Afelan_](https://mdoumoctar.bandcamp.com/album/afelan) | July 2013 | ![](https://tinyurl.com/yam6o2zh) |

And here's a Matt Miles quote [from _Dark Mountain_ issue 11]:
> The immigrants of the Global South, the cultures we've turned our backs on even as we profit from
> their labour, are the indicator species of our own societal collapse. The most sensitive and
> susceptible elements of our own species - the ones from whom everything has already been taken,
> the ones who have no recourse to technological mediation, whose subsistence economies have
> already been wrecked by globalization, whose land succumbs to the rising seas, whose societies
> have been destroyed by imperial land grabs and resource wars - they are here now, knocking on
> our front doors, because they have nowhere else to go. On a planet dominated by the movements of
> human beings, we are our own indicator species.
---
Made possible thanks to [Marked.js](https://marked.js.org/) and [Maxime Mangel](https://github.com/MangelMaxime/Fulma/blob/master/docs/src/Libs/Fable.Import.Marked.fs)."""
