# Build stage 0
FROM erlang:alpine

# Install Rebar3
RUN mkdir -p /buildroot/rebar3/bin
ADD https://s3.amazonaws.com/rebar3/rebar3 /buildroot/rebar3/bin/rebar3
RUN chmod a+x /buildroot/rebar3/bin/rebar3

# Setup Environment
ENV PATH=/buildroot/rebar3/bin:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang application
COPY editor editor

# And build the release
WORKDIR editor
RUN rebar3 as prod release


# Build stage 1
FROM alpine

 RUN apk add --no-cache openssl && \
     apk add --no-cache ncurses-libs

# Install the released application
COPY --from=0 /buildroot/editor/_build/prod/rel/editor /editor

# Expose relevant ports
EXPOSE 8080
#EXPOSE 8443

CMD ["/editor/bin/editor", "foreground"]