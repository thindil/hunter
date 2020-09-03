FROM docker.pkg.github.com/thindil/tashy/tashy:8.6.8
ENV TZ=Europe/Berlin
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone \
 && apt-get update && apt-get install -y libmagic-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
