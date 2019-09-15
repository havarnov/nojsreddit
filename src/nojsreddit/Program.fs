module nojsreddit.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open FSharp.Data
open FSharp.Control.Tasks.V2.ContextSensitive
open System.Net.Http
open Newtonsoft.Json.Linq
open Newtonsoft.Json
open System.Web

// ---------------------------------
// Data
// ---------------------------------

type SubReddit = JsonProvider<"""https://www.reddit.com/r/rust.json""">

// ---------------------------------
// Models
// ---------------------------------

type Comments = {
    Kind: string;
    Data: CommentsData
}
and CommentsData = {
    Children: Comment list
}
and Comment = {
    Data: CommentData
}
and CommentData = {
    Author: string;
    Score: int;
    Created_UTC: float;
    Body: string;
    Body_Html: string;
    Replies: Comments
}
and Post = {
    Title: string;
    SelfTextHtml: string option;
    Author: string;
    Score: int;
    Ups: int;
    Downs: int;
    Url: string;
}
and PostAndComments = {
    Post: Post;
    Comments: Comments;
}

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open GiraffeViewEngine

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "nojsreddit" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]

    let post (model: SubReddit.Child) =
        let link =
            if Option.isSome model.Data.Selftext then
                model.Data.Permalink
            else
                model.Data.Url
        let link = a [ _href link ] [ encodedText model.Data.Title ]
        let score = span [] [ encodedText (sprintf "%d points" model.Data.Score) ]
        let spacer = span [] [ encodedText " | "]
        let commentLink =
            a
                [ _href model.Data.Permalink ]
                [ encodedText (sprintf "%d comments" model.Data.NumComments) ]
        li [] [ link; spacer; score; spacer; commentLink; ]
    
    let subreddit (name: string option) (model: SubReddit.Root) =
        let displayName =
            match name with
            | Some name -> sprintf "/r/%s" name
            | None -> sprintf "Reddit - home page of the internet"
        let header = h1 [] [ encodedText displayName ]

        let afterLink =
            match name with
            | Some name ->
                sprintf "%s?after=%s" name model.Data.After
            | None ->
                sprintf "/?after=%s" model.Data.After

        let next =
            a [ _href afterLink ] [ encodedText "next"]

        let posts =
            model.Data.Children
            |> Array.map post
            |> Array.toList
            |> ul []

        [ header; posts; next; ]
        |> layout

    let rec comment (model: Comment) =
        let replies =
            try
                if model.Data.Replies.Kind = "more" then
                    ul [] [ li [] [encodedText "more..." ] ]
                else
                    model.Data.Replies.Data.Children
                    |> List.map comment
                    |> ul []
            with
                | _ -> ul [] []

        let headerId = (Guid.NewGuid ()).ToString()
        let header =
            if isNotNull model.Data.Author then
                label [ _for headerId ] [ encodedText (sprintf "%s (%d points)" model.Data.Author model.Data.Score) ]
            else
                label [ _for headerId ] [ encodedText "more..." ]
        let headerInput =
            input [ _type "checkbox"; _style "display: none"; _id headerId ]
        let body = rawText (HttpUtility.HtmlDecode model.Data.Body_Html)
        li [] [ headerInput; header; body; replies; ]

    let comments (model: Comments) =
        model.Data.Children
        |> List.map comment
        |> ul []

    let postAndComments (model: PostAndComments) =
        let header = h1 [] [ encodedText model.Post.Title ]
        let body =
            match model.Post.SelfTextHtml with
            | Some selfTextHtml ->
                let body = rawText (HttpUtility.HtmlDecode selfTextHtml)
                span [] [ body ]
            | None ->
                let link = a [ _href model.Post.Url ] [ encodedText model.Post.Url ]
                span [] [ link ]

        [ header; body; comments model.Comments; ]
        |> layout

// ---------------------------------
// Web app
// ---------------------------------


let subredditHandlerAbstract (subredditName: string option) =
    fun n (ctx: HttpContext) -> task {
        let url =
            match subredditName with
            | Some subredditName ->
                sprintf "https://reddit.com/r/%s.json" subredditName
            | None ->
                sprintf "https://reddit.com/.json"

        let url =
            match ctx.Request.Query.TryGetValue("after") with
            | (true, value) ->
                sprintf "%s?after=%s" url (value.ToString())
            | _ ->
                url
        let! subreddit = SubReddit.AsyncLoad url
        
        let view      = Views.subreddit subredditName subreddit
        return! htmlView view n ctx
    }

let indexHandler () =
    subredditHandlerAbstract None

let subredditHandler (subredditName: string) =
    subredditHandlerAbstract (Some subredditName)

let commentHandler (subredditName, commendId, title) =
    fun n ctx -> task {
        use httpClient = new HttpClient()
        let url = sprintf "https://reddit.com/r/%s/comments/%s/%s.json" subredditName commendId title
        let! response = httpClient.GetAsync(url)
        let! body = response.Content.ReadAsStringAsync()
        let json = JArray.Parse(body)
        let subreddit = SubReddit.Parse(json.[0].ToString())
        let post = subreddit.Data.Children.[0].Data
        let post = {
            Title = post.Title;
            SelfTextHtml = post.SelftextHtml;
            Author = post.Author;
            Score = post.Score;
            Ups = post.Ups;
            Downs = post.Downs;
            Url = post.Url;
        }
        let json = json.[1].ToString()
        let settings = JsonSerializerSettings()
        let comments =
            JsonConvert.DeserializeObject<Comments>(json, settings)
        let postAndComments = {
            Post = post;
            Comments = comments;
        }
        let view = Views.postAndComments postAndComments
        return! htmlView view n ctx
    }

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> warbler (fun _ -> indexHandler ())
                routef "/r/%s/comments/%s/%s" commentHandler 
                routef "/r/%s" subredditHandler
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true  -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseHttpsRedirection()
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddFilter(fun l -> l.Equals LogLevel.Error)
           .AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0