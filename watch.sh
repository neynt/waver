_build/default/hello.exe
while true; do
	inotifywait -e close_write _build/default/hello.exe
	time _build/default/hello.exe
done
