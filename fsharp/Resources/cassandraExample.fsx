open Cassandra
open System

let cluster = Cassandra.Cluster.Builder().AddContactPoint("127.0.0.1").Build();
let session = cluster.Connect("demo");

session.Execute(@"
    CREATE KEYSPACE IF NOT EXISTS inventory
    WITH replication = {'class': 'SimpleStrategy', 'replication_factor': 2};
")

session.Execute(@"USE inventory;")

session.Execute(@"

CREATE TABLE IF NOT EXISTS products ( 
	product_id text,
	sku text,
	title text,
	price_in_pence int,
	categories list<text>,
	stock int,
	PRIMARY KEY (product_id, sku) 
);

")

session.Execute(@"
insert into Products (product_id, sku, title, price_in_pence, categories, stock) values (
  '1', 'sku', 'egg', 466, ['food', 'good food'], 7  
);
")

let results = session.Execute("select * from products") 

let printOut(row: Row) = 
    Console.WriteLine(row.GetValue<string>("title"))
    ()

results |> Seq.iter printOut


let boundQuery = session.Prepare("select * from products where product_id=?")
let boundStatement = boundQuery.Bind("1")
session.Execute(boundStatement) |> Seq.iter printOut
