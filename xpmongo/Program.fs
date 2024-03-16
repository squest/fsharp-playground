open MongoDB.Driver

let connectionString = "mongodb://localhost:27017"
let client = new MongoClient(connectionString)
let database = client.GetDatabase("xpone")
let collection = database.GetCollection<BsonDocument>("authors")

// Gue mau bikin data isinya author
// bentuk datanya {:name "John Doe" :id 1 :username "johndoe" :email "johndoe@gmail.com"}

let author = 
    new BsonDocument(
        [
            "name", "John Doe";
            "id", 1;
            "username", "johndoe";
            "email", "johndoe@gmail.com"
        ])

// Insert data ke collection

