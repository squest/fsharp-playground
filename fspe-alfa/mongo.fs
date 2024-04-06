module Mongo

open MongoDB.Driver
open MongoDB.Bson
open System

let uuid () : string = Guid.NewGuid().ToString()

let connectionString = "mongodb://localhost:27017"
let client = MongoClient(connectionString)
let database = client.GetDatabase("xpone")
let collection = database.GetCollection<BsonDocument>("articles")

type Article = {
  Title: string
  Content: string
  _id: string
}

let article1 = { 
  Title = "The fuck is this article"; 
  Content = "This is the content" 
  _id = uuid()
  }

let articleDoc = BsonDocument()
bookDoc.Add("Title", article1.Title)
bookDoc.Add("Content", article1.Content)
bookDoc.Add("_id", article1._id)

let documents = collection.Find(FilterDefinition<BsonDocument>.Empty).ToList()

let print () =
  documents.ForEach(fun doc ->
    printfn "%s" (doc.ToString())
    )

let docs =
  documents.ForEach(fun doc ->
    
  )

