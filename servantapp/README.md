# servantapp
https://docs.servant.dev/en/stable/cookbook/testing/Testing.html
https://hspec.github.io/
<!-- QUERY PARAM request can run even if we don't pass parameters -->
<!--  :<|> "joe" :> QueryParam "name" Text :> Get '[JSON] Text -->

<!-- ROUTE PARAM request have to pass parameters otherwise 404 error -->
<!--  :<|> "foe" :> Capture "name" Text :> Get '[JSON] Text -->

<!-- CURL request POST RESP -->
<!-- curl -iv http://localhost:8081/respjoe/ -H 'Content-Type: application/json' -d '{"name":"jyoti"}' -->

hspec - Library for testing.
hspec-wai - library for API test'
QuickCheck - auto data generation for test case.
servant-quickcheck - API data generation.








