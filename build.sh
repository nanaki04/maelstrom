# fsharpc --target:library -r:UnityEngine.dll -r:UnityEditor.dll
fsharpc --target:library Types.fs Well.fs Maelstrom.fs WellGuardians.fs
mv WellGuardians.dll build/Maelstrom.dll
