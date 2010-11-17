all: build

build:
	erlc -pa ../ebin -W +compress +debug_info +strip -o ebin src/*.erl
	cp src/echo_app.app.src ebin/echo_app.app

clean:
	rm -f ebin/*.beam
	rm -f ebin/*.app

run_app: build
	erl -pa ../ebin \
        -pa ebin \
        -eval 'application:start(sasl)' \
        -eval 'application:start(echo_app)'

.PHONY: all build clean server app
