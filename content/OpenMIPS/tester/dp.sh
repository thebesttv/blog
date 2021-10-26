while true; do
    if make; then
        echo AC
    else
        echo WA
        read p
    fi
    sleep 1
done
