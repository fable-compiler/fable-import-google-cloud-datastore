module Fable.Import.Google.Cloud.Datastore

open Fable.Core

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Google.Cloud

type [<Erase>] DatastoreKey = DatastoreKey of obj
type [<Erase>] Kind = Kind of string
type [<Erase>] EntityId = EntityId of string

type Key = Key of Kind * EntityId
type KeyPath =
  | Ancestor of Key * KeyPath
  | Term of Key

module UnqualifiedKey =
  type [<Erase>] T = private T of string array

  let createFromKey (Key (Kind k, EntityId e)) =
    T [|k; e|]

  let createFromKeyPath keyPath =
    let rec loop agg kp =
      match kp with
      | Ancestor (Key (Kind k, EntityId e), nextKp) -> loop (e :: k :: agg) nextKp
      | Term (Key (Kind k, EntityId e)) -> e :: k :: agg
    loop [] keyPath |> Array.ofList |> Array.rev |> T

  let get (T x) = x

type DatastoreEntity<'a> =
  { key: DatastoreKey
    data: 'a }

type Options =
  { projectId: ProjectId option
    email: string option
    keyFilename: string option
    apiEndpoint: string option
    ``namespace``: string option }

module datastore_types =
  type Datastore =
    abstract key: UnqualifiedKey.T -> DatastoreKey
    abstract get: DatastoreKey -> JS.Promise<'a option>
    abstract save: DatastoreEntity<'a> -> JS.Promise<ApiResponse>

  type Globals =
    [<Emit("$0($1)")>]
    abstract Init : ?options:Options -> Datastore

[<Import("default","@google-cloud/datastore")>]
let datastore: datastore_types.Globals = jsNative
