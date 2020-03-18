[xml]$doc = Get-Content .\src\Directory.Build.props
$version = $doc.Project.PropertyGroup.VersionPrefix # the version under development, update after a release
$versionSuffix = '-build.0' # manually incremented for local builds

function isVersionTag($tag){
    $v = New-Object Version
    [Version]::TryParse($tag, [ref]$v)
}

if ($env:appveyor){
    $versionSuffix = '-build.' + $env:appveyor_build_number
    if ($env:appveyor_repo_tag -eq 'true' -and (isVersionTag($env:appveyor_repo_tag_name))){
        $version = $env:appveyor_repo_tag_name
        $versionSuffix = ''
    }
    Update-AppveyorBuild -Version "$version$versionSuffix"
}

dotnet build -c Release Chiron.sln /p:Version=$version$versionSuffix
dotnet test --no-build -c Release tests/Chiron.Tests/Chiron.Tests.fsproj
dotnet pack --no-build -c Release src/Chiron /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet benchmarks/Chiron.Benchmarks/bin/Release/netcoreapp3.1/Chiron.Benchmarks.dll --class SwaggerSchema
