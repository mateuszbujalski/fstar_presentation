dotnet build
dotnet run --no-build

Tests:
correct
{ "l" : 6 }
{ "_x" : 6, "_y" : "aaa", "_z" : true }

incorrect
{ "l" : 4 }
{ "_x" : 6, "_y" : "aaa", "_z" : true }

incorrect
{ "l" : 5 }
{ "_x" : 6, "_y" : "aaa", "_z" : false }

incorrect
{ "invalid" : 6 }
{ "_x" : 6, "_y" : "aaaaaaaaaaaaaaaaaaaaaa", "_z" : true }

