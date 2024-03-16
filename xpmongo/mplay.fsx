module mplay

#r "nuget: MongoDB.Driver"
#r "nuget: Newtonsoft.Json"

open Newtonsoft.Json
open MongoDB.Driver
open MongoDB.Bson
open System 


let connectionString = "mongodb://localhost:27017"
let client = MongoClient(connectionString)
let database = client.GetDatabase("xpone")
let authorCollection = database.GetCollection<BsonDocument>("authors")

// Gue mau bikin data isinya author
// bentuk datanya {:name "John Doe" :id 1 :username "johndoe"}
// Gue mau bikin pake data type f# baru abis itu diconvert ke BsonDocument

type Author = {
    _id: string
    Name: string
    Username: string
    Email: string
}

let insertAuthor (author: Author) =
// ini data author yg masuk ga pake _id, gue mau generate id baru pas masukin ke mongodb
    let updateAuthor = { author with _id = Guid.NewGuid().ToString() }
    let json = JsonConvert.SerializeObject(updateAuthor)
    let bson = BsonDocument.Parse(json)
    authorCollection.InsertOne(bson)

let allAuthors () : Author list =
    let filter = Builders<BsonDocument>.Filter.Empty
    let bsonAuthors = authorCollection.Find(filter).ToList() |> Seq.toList
    bsonAuthors |> List.map (fun doc ->
        {
            _id = doc["_id"].ToString(); // Ubah ke ToString untuk handling ObjectId
            Name = doc["Name"].AsString;
            Username = doc["Username"].AsString;
            Email = doc["Email"].AsString
        })




printfn "MongoDB setup is ready!"
