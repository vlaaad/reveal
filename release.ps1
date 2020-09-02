param ([Parameter(Mandatory)]$version)

function invoke {
    $exe, $argsForExe = $Args
    $ErrorActionPreference = 'Continue'
    try { & $exe $argsForExe } catch { Throw }
    if ($LASTEXITCODE) { Throw "$exe indicated failure (exit code $LASTEXITCODE; full command: $Args)." }
}

clj -A:build -m version $version
clj -Spom
invoke git commit -am "Release $version"
invoke git tag $version
invoke git push
invoke git push origin
clj -A:depstar "$version.jar"
$env:CLOJARS_USERNAME=(Read-Host -Prompt "Username")
$env:CLOJARS_PASSWORD=(Read-Host -Prompt "Token" -AsSecureString | ConvertFrom-SecureString)
clj -e clj -A:deploy "$version.jar"
rm "$version.jar"





