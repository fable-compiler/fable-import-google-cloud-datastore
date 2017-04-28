module Tests

open Fable.Core
open Fable.Import
open Util
open Fable.Import.Google.Cloud.Datastore

let tests () =
  describe "Google Datastore" <| fun _ ->
    it "exists" <| fun _ ->
      Assert.ok(JsInterop.datastore)
    describe "UnqualifiedKey" <| fun _ ->
      describe "createFromKeyPath" <| fun _ ->
        let testAncestorKind0, testAncestorEntityId0 = "ancestor0", "ancestorId0"
        let testAncestorKind1, testAncestorEntityId1 = "ancestor1", "ancestorId1"
        let testKind, testEntityId = "material", "testId"
        let terminal = Term( Key (Kind testKind, EntityId testEntityId))
        let ancestor1 = Ancestor( Key (Kind testAncestorKind1, EntityId testAncestorEntityId1), terminal)
        let ancestor0 = Ancestor( Key (Kind testAncestorKind0, EntityId testAncestorEntityId0), ancestor1)
        it "created for term only" <| fun () ->
          let actual = UnqualifiedKey.createFromKeyPath terminal
          let actualArray = UnqualifiedKey.get actual
          let expectedArray = [| testKind; testEntityId |]
          Assert.deepEqual(actualArray, expectedArray)
        it "created for ancestor & term" <| fun () ->
          let actual = UnqualifiedKey.createFromKeyPath ancestor1
          let actualArray = UnqualifiedKey.get actual
          let expectedArray = [| testAncestorKind1; testAncestorEntityId1; testKind; testEntityId |]
          Assert.deepEqual(actualArray, expectedArray)
        it "created for two ancestors & term" <| fun () ->
          let actual = UnqualifiedKey.createFromKeyPath ancestor0
          let actualArray = UnqualifiedKey.get actual
          let expectedArray = [| testAncestorKind0; testAncestorEntityId0; testAncestorKind1; testAncestorEntityId1; testKind; testEntityId |]
          Assert.deepEqual(actualArray, expectedArray)

tests ()
