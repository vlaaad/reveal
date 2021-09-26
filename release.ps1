function invoke {
    $exe, $argsForExe = $Args
    $ErrorActionPreference = 'Continue'
    try { & $exe $argsForExe } catch { Throw }
    if ($LASTEXITCODE) { Throw "$exe indicated failure (exit code $LASTEXITCODE; full command: $Args)." }
}
Write-Output "Deploying FREE version"
if (!((invoke git rev-parse --abbrev-ref HEAD) -eq "free")) {
    throw "not on free"
}
try { invoke git diff-index --quiet HEAD -- } catch { Throw "index is not clean" }

$version = "1.3.$(invoke git rev-list HEAD --count)"

clj -A:build -M -m version $version $(invoke git rev-parse HEAD)
clj -Spom
invoke git tag $version
invoke git push
invoke git push free-remote $version
clj -A:depstar "$version.jar"
clj -A:build -M -m deploy "$version.jar" (Read-Host -Prompt "Username") (Read-Host -Prompt "Token" -AsSecureString | ConvertFrom-SecureString -AsPlainText)
rm "$version.jar"
invoke git checkout -- pom.xml


