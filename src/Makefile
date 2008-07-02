default : all

all : backend frontend

opt : backend_opt frontend_opt

backend :
	cd utils ;\
	make all; \

	cd language; \
	make all; \

	cd compiler; \
	make all; \

	cd static_analysis; \
	make all; \

	cd servers; \
	make all; \

backend_opt :
	cd utils ;\
	make opt; \

	cd language; \
	make opt; \

	cd compiler; \
	make opt; \

	cd static_analysis; \
	make opt; \

	cd servers; \
	make opt; \

frontend :
	cd client; \
	make all; \

frontend_opt :
	cd client; \
	make opt; \

clean :
	cd utils; \
	make clean; \

	cd language; \
	make clean; \

	cd compiler; \
	make clean; \

	cd static_analysis; \
	make clean; \

	cd servers; \
	make clean; \

	cd client; \
	make clean; \
