module DownloadWallpapers

open System
open System.Net
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Runtime.Serialization
open System.Runtime.Serialization.Json

[<DataContract>] // Serialize Attributes
    type JsonResult = {
        [<field: DataMember(Name = "id")>]
        Id:int
    }

// Some kind of Enum
type PeriodOfTime = ThreeDays | OneWeek

// Convert Json to an Object of type 't
let jsonToObject<'t> (jsonString:string) : 't =
    use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(jsonString))
    let obj = (new DataContractJsonSerializer(typeof<'t>)).ReadObject(ms)
    // Cast obj to type t
    obj :?> 't


// Get Url for downloading Json
let getUrl (start: int) (period: PeriodOfTime) =
    // User pattern matching to check Enum
    let p = match period with
                | PeriodOfTime.ThreeDays -> "3d"
                | PeriodOfTime.OneWeek -> "1w"

    sprintf "http://wallbase.cc/toplist/%d/123/eqeq/0x0/0/111/60/%s" start p

// Downloads content from url
let getPageJson (url: string) =
        let req = WebRequest.CreateDefault(new Uri(url)) :?> HttpWebRequest // Cast up
        req.Headers.Add("X-Requested-With", "XMLHttpRequest")
        req.CookieContainer <- new CookieContainer()
        req.CookieContainer.SetCookies(new Uri(url), "is_adult=1")

        use res = req.GetResponse()
        use stream = new StreamReader(res.GetResponseStream())

        stream.ReadToEnd()


// Split string into a list of json object strings
let parseJson (input: string) =
    Regex.Split(input.Substring(1, input.Length - 2), "(?<=\}\})\,") |> List.ofArray


// Download html content and parse it. Returns F# Option type
let getImageUrl (imageId: int) =
    try
        use webClient = new WebClient()
        let html = webClient.DownloadString((sprintf "http://wallbase.cc/wallpaper/%d" imageId))
        let m = Regex.Match(html, (sprintf "<img src='(.*)(wallpaper-%d.*)'" imageId))

        if m.Success then Some(m.Groups.[1].Value, m.Groups.[2].Value) else None
    with
        | :? Exception -> None


// Download a file and save it to destination
let downloadUrl (url: string) (destination: string) =
    use webClient = new WebClient()
    webClient.DownloadFile(url, destination)


// Create Wallpaper directory
let di = Directory.CreateDirectory(Environment.CurrentDirectory + @"\Wallpaper")


let result =
    (getUrl 0 PeriodOfTime.ThreeDays)
        |> getPageJson
        |> parseJson
        |> List.map jsonToObject<JsonResult>
        |> List.map (fun x -> x.Id)
        |> List.map getImageUrl
        |> List.filter (fun x -> x.IsSome)
        |> List.iter (fun x ->
            let url, filename = x.Value
            downloadUrl (sprintf "%s%s" url filename) (sprintf @"%s\%s\%s" Environment.CurrentDirectory "Wallpaper" filename)
        )