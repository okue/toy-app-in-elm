OUTPUT=output
JS=${OUTPUT}/main.js
JSMIN=${OUTPUT}/main.min.js
COMPRESS=uglifyjs --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe'
MANGLE=uglifyjs --mangle

compile:
	elm make --optimize Main.elm --output=${JS}
	make compress

reactor:
	elm reactor

compress:
	${COMPRESS} ${JS} | ${MANGLE} --output=${JSMIN}
	@echo "Initial size: `cat ${JS} | wc -c` bytes  ${JS}"
	@echo "Minified size:`cat ${JSMIN} | wc -c` bytes  ${JSMIN}"
	@echo "Gzipped size: `cat ${JSMIN} | gzip -c | wc -c` bytes"

clean:
	rm output/*.js || true
