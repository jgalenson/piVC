
let _ =
  let executable = Utils.get_absolute_path "PiGui.jar" in
  let java = "java -jar " ^ executable in
  Sys.command java ;;
